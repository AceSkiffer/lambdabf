module Parser 
    (
        parse,
        BfInstruction (..),
        BfAstItem (..)
    )
    where 

data BfInstruction = INC | DEC | MR | ML | PRT| RD | LB | LE | DEP
    deriving (Show)

data BfAstItem = BfAstItem BfInstruction Int | 
                 BfLoopAstItem [BfAstItem] |
                 BfBeginAstItem Int |
                 BfEndAstItem Int |
                 BfEmptyAstItem Int
    deriving (Show)

parse :: String -> [BfAstItem]
parse "" = [BfBeginAstItem (-1), BfEndAstItem 0]
parse source = parseNextWithResult (BfBeginAstItem (-1)) (-1) source
    
parseNextWithResult :: BfAstItem -> Int -> String -> [BfAstItem]
parseNextWithResult (BfEmptyAstItem _) pos source = parseNext pos source
parseNextWithResult item pos source = item : parseNext pos source

parseNext :: Int -> String -> [BfAstItem]
parseNext pos source = let pos' = pos+1 in 
                           parseSymbol (getCharOrEmpty source pos') pos' source

getCharOrEmpty :: String -> Int -> Char
getCharOrEmpty s i 
    | i >= 0 && i < (length s) = s !! i
    | otherwise = '\0'

parseSymbol :: Char -> Int -> String -> [BfAstItem]
parseSymbol '\0' p = endParsing p
parseSymbol '+' p = parseNextWithResult (BfAstItem INC p) p
parseSymbol '-' p = parseNextWithResult (BfAstItem DEC p) p
parseSymbol '>' p = parseNextWithResult (BfAstItem MR p) p
parseSymbol '<' p = parseNextWithResult (BfAstItem ML p) p
parseSymbol '[' p = parseLoop p
parseSymbol ']' p = endLoopParsing p
parseSymbol '.' p = parseNextWithResult (BfAstItem PRT p) p
parseSymbol ',' p = parseNextWithResult (BfAstItem RD p) p
parseSymbol ';' p = skipComment p
parseSymbol '#' p = parseNextWithResult (BfAstItem DEP p) p
parseSymbol _   p = parseNext p

endParsing :: Int -> String -> [BfAstItem]
endParsing pos source = [BfEndAstItem pos]

parseLoop :: Int -> String -> [BfAstItem]
parseLoop pos source = let loopTokens = parseNextWithResult (BfAstItem LB pos) pos source in
                           parseNextWithResult (BfLoopAstItem loopTokens) ((getPos . last) loopTokens) source
    where
        getPos :: BfAstItem -> Int
        getPos (BfAstItem _ p) = p
        getPos (BfBeginAstItem p) = p
        getPos (BfEndAstItem p) = p
        getPos (BfEmptyAstItem p) = p
        getPos (BfLoopAstItem items) = getPos (last items)                

endLoopParsing :: Int -> String -> [BfAstItem]
endLoopParsing pos source = [BfAstItem LE pos]

skipComment :: Int -> String -> [BfAstItem]
skipComment pos source = if pos < (length source) && (source !! pos) /= '\n'
                         then skipComment (pos+1) source
                         else parseNextWithResult (BfEmptyAstItem (pos+1)) pos source
