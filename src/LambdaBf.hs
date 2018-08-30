module LambdaBf
    ( 
        interpretFromText
    ) 
    where

import Parser (parse)
import Interpreter (interpret, emptyTape)
import Utils (skipValue)

interpretFromText :: String -> Int -> IO ()
interpretFromText source cellSize = skipValue $ interpret (parse source) (emptyTape (cellSize - 1))
