{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Version (showVersion)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Control.DeepSeq (force)

import Paths_lambdabf (version)

import LambdaBf
import Utils (isPowerOfTwo)

main :: IO ()
main = getArgs >>= handleCommandLineParams

main' :: String -> IO ()
main' argsString = handleCommandLineParams $ words argsString

defaultCellSize :: Int
defaultCellSize = 256

handleCommandLineParams :: [String] -> IO ()
handleCommandLineParams args = 
    case args of
        ["-h"]                        -> printHelp
        ["--help"]                    -> printHelp
        ["-v"]                        -> printVersion
        ["--version"]                 -> printVersion
        []                            -> interpretFromStdin defaultCellSize
        ("-c":value:[])               -> checkCellSizeArgument (read value) >>= interpretFromStdin
        ("--cell-size":value:[])      -> checkCellSizeArgument (read value) >>= interpretFromStdin
        (file:[])                     -> interpretFromFile file defaultCellSize
        ("-c":value:file:[])          -> checkCellSizeArgument (read value) >>= interpretFromFile file
        ("--cell-size":value:file:[]) -> checkCellSizeArgument (read value) >>= interpretFromFile file
        _                             -> printUnknownArgumetsError

printHelp :: IO ()
printHelp = printVersion >> putStrLn "Usage: lambdabf.exe [option] [file | -]\n\
                                     \Options and arguments:\n\
                                     \-h, --help       : print this help message and exit\n\
                                     \-v, --version    : print the lambdabf version number and exit\n\
                                     \-c, --cell-size  : set the cell size in bits (default 256). Should be a power of 2\n\
                                     \file             : program read from file\n\
                                     \-                : program read from stdin"

printVersion :: IO ()
printVersion = putStrLn $ "lambdabf " ++ showVersion version

printUnknownArgumetsError :: IO ()
printUnknownArgumetsError = putStrLn "Unknown argument(s)\n\
                                     \Try 'lambdabf -h' for more information."

interpretFromStdin :: Int -> IO ()
interpretFromStdin cellSize = putStr "lbf>" >> hFlush stdout >> getLine >>= (\source -> interpretFromText source cellSize)

interpretFromFile :: String -> Int -> IO ()
interpretFromFile filePath cellSize = readFile filePath >>= (\source -> interpretFromText source cellSize)

checkCellSizeArgument :: Int -> IO (Int)
checkCellSizeArgument v = if isPowerOfTwo v
                          then return v
                          else error $ "Error: the cell size should be a power of 2"
