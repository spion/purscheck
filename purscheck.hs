module Main (main) where

import System.Process (runProcess, waitForProcess)
import System.Environment (getArgs)
import qualified Data.List as List
import qualified System.FilePath.Glob as Glob
import qualified System.FilePath as Path
import qualified System.Directory as Dir

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tmpFile, originalFile, output] -> compile tmpFile originalFile output
    _ -> putStrLn "Usage: purscheck <tempFile> <originalFile> <output>"

compile :: FilePath -> FilePath -> FilePath -> IO ()
compile tmpFile originalFile output = do
  let initialDir = Path.dropFileName originalFile
  maybeConfigFile       <- findConfig initialDir
  case maybeConfigFile of
    Nothing -> putStrLn "Unable to find .purescript-paths"
    Just configFile -> do
      configContent    <- readFile configFile
      let globExpander = expandGlobFrom $ Path.takeDirectory configFile
      let globs        = lines configContent
      includeFiles     <- mapM globExpander globs
      let list         = makeList (concat includeFiles) tmpFile originalFile
      runPsc $ list ++ ["-o", output]


-- Path.takeDirectory always returns the directory of a given path. It cannot
-- safely be used instead of parentDir, as it returns the same path at the top
-- level directory "/".

-- The right signature for parentDir is therefore FilePath -> Maybe FilePath

parentDir :: FilePath -> Maybe FilePath
parentDir path =
  let directory = Path.takeDirectory path
  in if directory == path then Nothing else Just directory

findConfig :: FilePath -> IO (Maybe FilePath)
findConfig path = do
  let configInThisDirectory = Path.combine path ".purescript-paths"
  exists <- Dir.doesFileExist configInThisDirectory
  if exists then return $ Just configInThisDirectory else case parentDir path of
    Nothing     -> return Nothing
    Just parent -> findConfig parent

expandGlobFrom :: FilePath -> String -> IO [FilePath]
expandGlobFrom from pattern =
  Glob.globDir1 (Glob.compile pattern) from

makeList :: [FilePath] -> FilePath -> FilePath -> [FilePath]
makeList paths toAdd toRemove =
  toAdd : List.delete toRemove paths

runPsc :: [String] -> IO ()
runPsc args = do
  handle <- runProcess "psc" args Nothing Nothing Nothing Nothing Nothing
  waitForProcess handle
  return ()

