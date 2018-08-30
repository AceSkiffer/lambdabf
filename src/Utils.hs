module Utils 
    (
        skipValue,
        wrapRightPower2,
        isPowerOfTwo
    )
    where

import Data.Bits

skipValue :: Monad m => m a -> m ()
skipValue m = m >> (return ())

wrapRightPower2 :: Int -> Int -> Int
wrapRightPower2 value rightBound = if rightBound > 0 && isPowerOfTwo (rightBound + 1)
                                   then wrap value rightBound
                                   else error $ "value should be positive number and the right \
                                                \bound should be (2^n - 1)"
    where
        wrap :: Int -> Int -> Int
        wrap v rB 
            | v == 0 = 0
            | v > 0 = v .&. rB
            | otherwise = (abs v) `xor` rB
isPowerOfTwo :: Int -> Bool
isPowerOfTwo x 
    | x <= 0 = False
    | otherwise = (x .&. (x-1)) == 0
