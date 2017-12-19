module InteroNixShim.Main where

import           Control.Monad        (when)
import           Data.Foldable        (find, traverse_)
import           Data.List            (stripPrefix)
import qualified Data.List.Split      as S
import           Data.Maybe           (catMaybes, fromMaybe, maybe)
import           Data.Semigroup       ((<>))
import           Options.Applicative
import           System.Directory
import           System.Environment   (getExecutablePath)
import           System.FilePath
import qualified System.Posix.Escape  as Posix
import           System.Posix.Process (executeFile)

foreign import ccall "silence_stderr" silenceStderr :: IO ()

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
run (Exec cmd) = do
  intero <- findInteroExec
  let absCmd =
        case cmd of
          "intero":t -> intero : t
          xs -> xs
  when
    (cmd == ["intero", "--version"])
    silenceStderr -- https://github.com/michalrus/intero-nix-shim/issues/1 https://github.com/NixOS/nix/issues/1341
  nixExec absCmd
run (Ghci opt) = do
  cabal <- findCabalExec
  intero <- findInteroExec
  let ghcSubst =
        maybe
          []
          (\p ->
             [ "--with-ghc"
             , if p == "intero"
                 then intero
                 else p
             ])
          (withGhc opt)
  let ghcOpts = (\o -> ["--ghc-options", o]) =<< ghcOptions opt
  -- Workaround for https://github.com/haskell/cabal/issues/4602 in Cabal 2.×
  (projectName, availableTargets) <- ideTargets'
  let libTarget = "lib:" ++ projectName
      defaultTargets =
        if libTarget `elem` availableTargets
          then [libTarget]
          else take 1 availableTargets
      targets' =
        case targets opt of
          [] -> defaultTargets
          [t]
            -- By default intero specifies just the package name as the target;
            -- stack handles this by loading the library and all executables, excluding
            -- tests and benchmarks. cabal repl can't handle multiple targets, so we
            -- can't do much better than just using the default target.
           ->
            if t == projectName
              then defaultTargets
            -- Strip project name prefix from stack target before using as cabal component
              else [fromMaybe t $ stripPrefix (projectName ++ ":") t]
          _:_:_ ->
            error
              "intero does not support using multiple targets at once \
                         \when using intero-nix-shim instead of stack"
  -- Important: do NOT pass `--verbose=0` to `cabal repl` or users’ errors won’t be shown in Flycheck.
  nixExec $ [cabal, "repl"] ++ ghcSubst ++ ghcOpts ++ targets'
run Path = putStrLn =<< rootDir
run IdeTargets = do
  (name, targets') <- ideTargets'
  let mix = (++) (name ++ ":") <$> targets'
  traverse_ putStrLn mix

nixExec :: [String] -> IO ()
nixExec cmd = do
  setCurrentDirectory =<< rootDir
  executeFile
    "nix-shell"
    True
    [ "--pure"
    , "--no-build-output"
    , "--quiet"
    , "--run"
    , "exec " ++ Posix.escapeMany cmd
    ]
    Nothing

findCabalExec :: IO FilePath
findCabalExec = findInLibExec "cabal"

findInteroExec :: IO FilePath
findInteroExec = findInLibExec "intero"

findInLibExec :: String -> IO FilePath
findInLibExec name = do
  me <- canonicalizePath =<< getExecutablePath
  libexec <- canonicalizePath $ takeDirectory me </> ".." </> "libexec"
  x <- findExecutablesInDirectories [libexec] name
  case x of
    res:_ -> return res
    _ -> error $ "No ‘" ++ name ++ "’ found in ‘" ++ libexec ++ "’."

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
      mf <-
        find
          (\f -> takeExtension f == ".cabal" && (not . null $ takeBaseName f)) <$>
        listDirectory dir
      return $ combine dir <$> mf

iterateUntilRepeated :: Eq a => (a -> a) -> a -> [a]
iterateUntilRepeated f a0 = reverse $ loop a0 []
  where
    loop an acc =
      let an1 = f an
      in if an == an1
           then acc
           else loop an1 (an1 : acc)

ideTargets' :: IO (String, [String])
ideTargets' = ideTargets <$> (readFile =<< cabalFile)

-- FIXME: yaml/regex/attoparsec?
ideTargets :: String -> (String, [String])
ideTargets cabal =
  let lns = lines cabal
      splits = S.split (S.condense . S.dropDelims $ S.oneOf " :") <$> lns
      kvs =
        splits >>= \case
          k:v:_ -> [(k, v)]
          _ -> []
      name = fromMaybe "_" $ snd <$> find (\(k, _) -> k == "name") kvs
      lib = ["lib:" ++ name | "library" `elem` lns]
      tpe s l = (++) (s ++ ":") . snd <$> filter (\(k, _) -> k == l) kvs
      exe = tpe "exe" "executable"
      test = tpe "test" "test-suite"
  in (name, lib ++ exe ++ test)

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
