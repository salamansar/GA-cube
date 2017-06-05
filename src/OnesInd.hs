{-# LANGUAGE NamedFieldPuns #-}
module OnesInd where

import Data.Bits
import BitOperations
import Individual

data BSInd = OnesInd {genome :: Int, bitLength :: Int}
   | ZerosInd {genome :: Int, bitLength :: Int}
   deriving Show
   

instance Individual BSInd where
   fitnesse ind@OnesInd {genome = g} 
      | g == 0 = 0
      | otherwise = let digit  = 1 .&. g
         in digit + fitnesse ind {genome = shiftR g 1}
   fitnesse ind@ZerosInd {genome = g, bitLength = bl}
      | bl == 0 = 0
      | otherwise = let zeroNum = 1 `xor` (1 .&. g)
         in zeroNum + fitnesse ZerosInd {genome = shiftR g 1, bitLength = bl - 1}
   maxLocale ind = bitLength ind
   mutate = mutateBitString
   crossover = crossoverBitString
   
instance BitStringInd BSInd where
   getGenome ind = genome ind
   setGenome ind newG = ind{genome = newG}
   
createOnesInd :: Int -> Int -> BSInd
createOnesInd g bl = OnesInd {genome = g, bitLength = bl}

createZerosInd :: Int -> Int -> BSInd
createZerosInd g bl = ZerosInd {genome = g, bitLength = bl}
   

      

   

