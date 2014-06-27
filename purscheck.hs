{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import System.Process
import System.Environment (getArgs)
import System.IO (readFile)
import qualified Data.List as List
import qualified System.FilePath.Glob as Glob
import qualified System.FilePath as Path
import qualified System.Directory as Dir

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tmpFile, originalFile, output] -> compile tmpFile originalFile output
    _ -> putStrLn "Invalid arguments"


compile :: FilePath -> FilePath -> FilePath -> IO ()
compile tmpFile originalFile output = do
  let initialDir = Path.dropFileName originalFile
  configFile       <- findConfig initialDir
  configContent    <- readFile configFile
  let globExpander = (expandGlobFrom $ Path.takeDirectory configFile)
  let globs        = lines configContent
  includeFiles     <- mapM globExpander globs
  let list         = makeList (concat includeFiles) tmpFile originalFile
  runPsc $ list ++ ["-o", output]

findConfig :: FilePath -> IO FilePath
findConfig path = do
  let configInThisDirectory = path ++ "/" ++ ".purescript-paths"
  exists <- Dir.doesFileExist configInThisDirectory
  case exists of
    True  -> return configInThisDirectory
    False -> findConfig $ Path.takeDirectory path

expandGlobFrom :: FilePath -> String -> IO [FilePath]
expandGlobFrom from pattern = do
  Glob.globDir1 (Glob.compile pattern) from

makeList :: [FilePath] -> FilePath -> FilePath -> [FilePath]
makeList paths toAdd toRemove =
  toAdd : (List.delete toRemove paths)

runPsc :: [String] -> IO ()
runPsc args = do
  handle <- runProcess "psc" args Nothing Nothing Nothing Nothing Nothing
  waitForProcess handle
  return ()
