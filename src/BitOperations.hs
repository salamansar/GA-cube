{-#LANGUAGE LambdaCase #-}
module BitOperations where

import Data.Bits
import Data.List

powOfTwo :: Int -> Int
powOfTwo num
   | num == 0 = 1
   | otherwise = shiftL 1 num
   
-- determines minimal bits number for number representation
minDimension :: Int -> Int
minDimension 0 = 0
minDimension num = 1 + (minDimension $ shiftR num 1)

bitValue :: Int -> Int -> Int
bitValue val position = case ((.&.val).powOfTwo) position of
   0 -> 0
   otherwise -> 1
   
bitsAsNum :: Int -> Int
bitsAsNum bitMask = foldr grabToNum 0 $ unfoldr splitToArray bitMask
   where splitToArray = (\case
                          0 ->  Nothing
                          x -> Just (x `mod` 10, x `div` 10))
         grabToNum = (\current accum -> (.|.) current $ shiftL accum 1)

numAsBits :: Int -> Int
numAsBits num = foldr collapseArray 0 $ unfoldr splitToArray num
   where splitToArray = (\case
                          0 -> Nothing
                          x -> Just ( x .&. 1, x `shiftR` 1))
         collapseArray = (\current accum -> accum * 10 + current)
         
numAsBits2 :: Int -> Int -> String
numAsBits2 count =  reverse.foldr (++) "".map show.take count.bitArray

bitArray :: Int -> [Int]
bitArray num = num.&.1 : bitArray (num `shiftR` 1)
         
inverseBit :: Int -> Int -> Int
inverseBit num n = num `xor` (powOfTwo $ n)

ones :: Int -> Int
ones 0 = 0
ones n = 1 .|. (shiftL (ones $ n-1) 1)

zeros :: Int -> Int
zeros = complement.ones

high :: Int -> Int -> Int
high num locale = num .&. (zeros $ locale)

highs :: Int -> Int -> Int
highs num locale = num .&. (zeros $ locale + 1)

low :: Int -> Int -> Int
low num locale = num .&. (ones $ locale + 1)

lows :: Int -> Int -> Int
lows num locale = num .&. (ones $ locale)

splitBits :: Int -> Int -> (Int, Int)
splitBits num locale = (highs num locale, low num locale)

         
