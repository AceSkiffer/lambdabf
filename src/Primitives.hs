module Primitives
    (
        bfPrint,
        bfPrintValue,
        bfRead
    ) 
    where

import System.IO (hFlush, stdout)
import Data.Char (ord, chr)

bfPrint :: Int -> IO ()
bfPrint value = putChar (chr value) >> hFlush stdout

bfPrintValue :: Int -> IO ()
bfPrintValue value = print (id value) >> hFlush stdout

bfRead :: IO (Int)
bfRead = getChar >>= return . ord
