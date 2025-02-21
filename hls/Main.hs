module Main (main) where

import System.Directory
import System.Environment
import System.FilePath
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Posix.Files
import System.Posix.Types
import Control.Monad
import Text.Printf
import Data.List
import System.IO
import Data.Bits ((.&.))

data Options = Options
    { showAll :: Bool
    , showLong :: Bool
    , showHelp :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { showAll = False
    , showLong = False
    , showHelp = False
    }

parseArgs :: [String] -> Options
parseArgs = foldl parseArg defaultOptions
  where
    parseArg opts "-a" = opts { showAll = True }
    parseArg opts "-l" = opts { showLong = True }
    parseArg opts "-h" = opts { showHelp = True }
    parseArg opts "--help" = opts { showHelp = True }
    parseArg opts _ = opts

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: ls [OPTION]..."
    putStrLn "List information about files in the current directory."
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  -a        show hidden files"
    putStrLn "  -l        use long listing format"
    putStrLn "  -h        display this help and exit"
    putStrLn "  --help    display this help and exit"

formatFileSize :: Integer -> String
formatFileSize size
    | size < 1024 = show size ++ "B"
    | size < 1024^2 = printf "%.1fK" (fromIntegral size / 1024 :: Float)
    | size < 1024^3 = printf "%.1fM" (fromIntegral size / (1024^2) :: Float)
    | otherwise = printf "%.1fG" (fromIntegral size / (1024^3) :: Float)

formatPermissions :: FileStatus -> String
formatPermissions status =
    [if isDirectory status then 'd' else '-'] ++
    [if fileMode status .&. ownerReadMode /= 0 then 'r' else '-'] ++
    [if fileMode status .&. ownerWriteMode /= 0 then 'w' else '-'] ++
    [if fileMode status .&. ownerExecuteMode /= 0 then 'x' else '-'] ++
    [if fileMode status .&. groupReadMode /= 0 then 'r' else '-'] ++
    [if fileMode status .&. groupWriteMode /= 0 then 'w' else '-'] ++
    [if fileMode status .&. groupExecuteMode /= 0 then 'x' else '-'] ++
    [if fileMode status .&. otherReadMode /= 0 then 'r' else '-'] ++
    [if fileMode status .&. otherWriteMode /= 0 then 'w' else '-'] ++
    [if fileMode status .&. otherExecuteMode /= 0 then 'x' else '-']

listFiles :: Options -> IO ()
listFiles opts = do
    if showHelp opts
        then printHelp
        else do
            currentDir <- getCurrentDirectory
            files <- getDirectoryContents currentDir
            let filteredFiles = if showAll opts
                               then files
                               else filter (not . isPrefixOf ".") files
            if showLong opts
                then do
                    forM_ filteredFiles $ \file -> do
                        status <- getFileStatus file
                        timeStr <- formatModificationTime status
                        let perms = formatPermissions status
                            size = formatFileSize (fromIntegral $ fileSize status)
                        printf "%s %8s %s %s\n" perms size timeStr file
                else putStr $ unlines filteredFiles

formatModificationTime :: FileStatus -> IO String
formatModificationTime status = do
    let modTime = posixSecondsToUTCTime $ realToFrac $ modificationTime status
    return $ formatTime defaultTimeLocale "%b %e %H:%M" modTime

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "args: " ++ show args
    let options = parseArgs args
    listFiles options