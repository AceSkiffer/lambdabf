module Interpreter 
    (
        interpret,
        emptyTape
    )
    where 

import Parser
import Primitives
import Utils (wrapRightPower2)

data Tape a = Tape [a] a [a] Int

emptyTape :: Int -> Tape Int
emptyTape = Tape zeros 0 zeros
    where
        zeros :: [Int]
        zeros = repeat 0 

getCell :: Tape Int -> Int
getCell (Tape _ a _ _) = a

setCell :: Tape Int -> Int -> Tape Int
setCell (Tape l a r maxValue) value = Tape l (wrapRightPower2 value maxValue) r maxValue

moveRight :: Tape Int -> Tape Int
moveRight (Tape ls v (r:rs) mv) = Tape (v:ls) r rs mv

moveLeft :: Tape Int -> Tape Int
moveLeft (Tape (l:ls) v rs mv) = Tape ls l (v:rs) mv

incCell :: Tape Int -> Tape Int
incCell t = setCell t (((+1) . getCell) t)

decCell :: Tape Int -> Tape Int
decCell t = setCell t (((subtract 1) . getCell) t)

printCell :: Tape Int -> IO (Tape Int)
printCell t = (bfPrint . getCell) t >> return t

printCellValue :: Tape Int -> IO (Tape Int)
printCellValue t = (bfPrintValue . getCell) t >> return t 

readCell :: Tape Int-> IO (Tape Int)
readCell t = bfRead >>= return . (setCell t)

startLoop :: [BfAstItem] -> Tape Int -> IO (Tape Int)
startLoop ais@(BfAstItem LB _:is) t = if (getCell t) == 0
                                      then return t
                                      else interpretLoop is t >>= restartLoopIfNeed
    where
        restartLoopIfNeed :: Tape Int -> IO (Tape Int)
        restartLoopIfNeed t
            | (getCell t) == 0 = return t
            | otherwise = startLoop ais t
                      
interpretLoop :: [BfAstItem] -> Tape Int -> IO (Tape Int)
interpretLoop (BfAstItem LE _:[]) t = return t
interpretLoop ((BfEndAstItem pos):[]) t = error $ "expect the loop end token (']') at " ++ (show (pos+1))
interpretLoop (item:items) t = interpretInstruction t item >>= interpretLoop items

interpret :: [BfAstItem] -> Tape Int -> IO (Tape Int)
interpret ((BfEndAstItem _):[]) t = return t
interpret ((BfAstItem LE pos):[]) t = error $ "unexpected the loop end token (']') at " ++ (show (pos+1))
interpret (i:[]) t = error $ "expect BfEndAstItem: get '" ++ (show i) ++ "'"
interpret (item:items) t = interpretInstruction t item >>= interpret items

interpretInstruction :: Tape Int -> BfAstItem -> IO (Tape Int)
interpretInstruction t (BfBeginAstItem _) = return t
interpretInstruction t (BfEndAstItem _) = return t
interpretInstruction t (BfEmptyAstItem _) = error "can't evaluate BfEmptyAstItem"
interpretInstruction t (BfAstItem INC _) = return $ incCell t
interpretInstruction t (BfAstItem DEC _) = return $ decCell t
interpretInstruction t (BfAstItem MR _) = return $ moveRight t
interpretInstruction t (BfAstItem ML _) = return $ moveLeft t
interpretInstruction t (BfAstItem LB pos) = error $ "expect BfLoopAstItem: get the loop begin token at " ++ (show (pos+1))
interpretInstruction t (BfAstItem LE pos) = error $ "unexpected the loop end token at " ++ (show (pos+1))
interpretInstruction t (BfAstItem PRT _) = printCell t
interpretInstruction t (BfAstItem RD _) = readCell t
interpretInstruction t (BfLoopAstItem items) = startLoop items t
interpretInstruction t (BfAstItem DEP _) = printCellValue t
