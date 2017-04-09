module Main where

import           Data.Semigroup      ((<>))
import           Debug.Trace         (trace)
import           Options.Applicative

data Command
  = Ghci { withGhc :: Maybe String
        ,  ghcOptions :: [String]
        ,  targets :: [String]}
  | Exec { execCommand :: [String]}
  | Path
  | IdeTargets
  deriving (Show)

main :: IO ()
main = run =<< render <$> execParser (info (parse <**> helper) fullDesc)

parse :: Parser Command
parse =
  hsubparser
    (command
       "ghci"
       (info
          (Ghci <$> optional (strOption (long "with-ghc")) <*>
           ((++) <$> many (strOption (long "ghci-options")) <*>
            many (strOption (long "ghc-options"))) <*
           optional (strOption (long "docker-run-args")) <*
           optional (switch (long "no-build")) <*
           optional (switch (long "no-load")) <*
           verbosity <*>
           many (argument str (metavar "TARGET…")))
          fullDesc) <>
     command
       "exec"
       (info
          (Exec <$ verbosity <*> some (argument str (metavar "CMD…")))
          fullDesc) <>
     command "path" (info (Path <$ flag' () (long "project-root")) fullDesc) <>
     command
       "ghc"
       (info
          (Exec <$> ((:) "ghc" <$> many (argument str (metavar "ARG…"))))
          fullDesc) <>
     command
       "ide"
       (info
          (hsubparser (command "targets" (info (pure IdeTargets) fullDesc)))
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

render :: Command -> [String]
render cmd = trace (show cmd) ["ble"]

run :: [String] -> IO ()
run cmd = putStrLn $ "nix-exec " ++ show cmd
