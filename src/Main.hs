module Main where

import           Data.Foldable        (find, traverse_)
import qualified Data.List.Split      as S
import           Data.Maybe           (catMaybes, fromMaybe, maybe)
import           Data.Semigroup       ((<>))
import           Options.Applicative
import           System.Directory
import           System.FilePath
import qualified System.Posix.Escape  as Posix
import           System.Posix.Process (executeFile)

data Command
  = Ghci GhciOpts
  | Exec [String]
  | Path
  | IdeTargets
  deriving (Show)

data GhciOpts = GhciOpts
  { withGhc :: Maybe String
  , ghcOptions :: [String]
  , targets :: [String]
  } deriving (Show)

main :: IO ()
main = run =<< execParser (info (parse <**> helper) fullDesc)

run :: Command -> IO ()
run (Exec cmd) = nixExec $ ["cabal", "exec", "--verbose=0", "--"] ++ cmd
run (Ghci opt) =
  nixExec $
  ["cabal", "repl", "--verbose=0"] ++
  maybe [] (\p -> ["--with-ghc", p]) (withGhc opt) ++
  ((\o -> ["--ghc-options", o]) =<< ghcOptions opt) ++ targets opt
run Path = putStrLn =<< rootDir
run IdeTargets = traverse_ putStrLn =<< ideTargets <$> (readFile =<< cabalFile)

nixExec :: [String] -> IO ()
nixExec cmd = do
  setCurrentDirectory =<< rootDir
  executeFile
    "nix-shell"
    True
    ["--pure", "--quiet", "--run", "exec " ++ Posix.escapeMany cmd]
    Nothing

rootDir :: IO FilePath
rootDir = takeDirectory <$> cabalFile

cabalFile :: IO FilePath
cabalFile = do
  searchDirs <- ancestors <$> getCurrentDirectory
  results <- catMaybes <$> traverse findCabal searchDirs -- FIXME: suboptimal…
  case results of
    cabal:_ -> return cabal
    _ -> error "No *.cabal file found."
  where
    ancestors d = d : iterateUntilRepeated takeDirectory d
    findCabal :: FilePath -> IO (Maybe FilePath)
    findCabal dir = do
      mf <- find (\f -> takeExtension f == ".cabal") <$> listDirectory dir
      return $ combine dir <$> mf

iterateUntilRepeated
  :: Eq a
  => (a -> a) -> a -> [a]
iterateUntilRepeated f a0 = reverse $ loop a0 []
  where
    loop an acc =
      let an1 = f an
      in if an == an1
           then acc
           else loop an1 (an1 : acc)

-- FIXME: yaml/regex/attoparsec?
ideTargets :: String -> [String]
ideTargets cabal =
  let lns = lines cabal
      splits = S.split (S.condense . S.dropDelims $ S.oneOf " :") <$> lns
      kvs =
        splits >>= \case
          k:v:_ -> [(k, v)]
          _ -> []
      name = fromMaybe "_" $ snd <$> find (\(k, _) -> k == "name") kvs
      lib = ["lib" | "library" `elem` lns]
      tpe s l = (++) (s ++ ":") . snd <$> filter (\(k, _) -> k == l) kvs
      exe = tpe "exe" "executable"
      test = tpe "test" "test-suite"
  in (++) (name ++ ":") <$> (lib ++ exe ++ test)

parse :: Parser Command
parse =
  hsubparser
    (command
       "ghci"
       (info
          (Ghci <$>
           (GhciOpts <$> optional (strOption (long "with-ghc")) <*>
            ((++) <$> many (strOption (long "ghci-options")) <*>
             many (strOption (long "ghc-options"))) <*
            optional (strOption (long "docker-run-args")) <*
            optional (switch (long "no-build")) <*
            optional (switch (long "no-load")) <*
            verbosity <*>
            many (argument str (metavar "TARGET…"))))
          fullDesc) <>
     command
       "exec"
       (info
          (Exec <$ verbosity <*> some (argument str (metavar "CMD…")))
          fullDesc) <>
     command
       "path"
       (info (Path <$ flag' () (long "project-root") <* verbosity) fullDesc) <>
     command
       "ghc"
       (info
          (Exec <$> ((:) "ghc" <$> many (argument str (metavar "ARG…"))) <*
           verbosity)
          fullDesc) <>
     command
       "ide"
       (info
          (hsubparser
             (command "targets" (info (IdeTargets <$ verbosity) fullDesc)))
          fullDesc) <>
     command
       "hoogle"
       (info
          (Exec <$>
           ((:) "hoogle" <$ verbosity <* optional (switch (long "no-setup")) <*>
            ((\xs ->
                if null xs
                  then ["--help"]
                  else xs) <$>
             many (argument str (metavar "ARG…")))))
          fullDesc))
  where
    verbosity = optional (strOption (long "verbosity"))
