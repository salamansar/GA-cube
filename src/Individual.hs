module Individual where 

import Data.Bits
import BitOperations
   
class Individual i where
   fitnesse :: i -> Int
   mutate :: i -> Int -> i
   crossover :: (i, i) -> Int -> (i,i)
   maxLocale :: i -> Int
   
class Individual bsi => BitStringInd bsi where
   getGenome :: bsi -> Int
   setGenome :: bsi -> Int -> bsi
   
class HasPhenotype i where
   phenotype :: i -> a

-- Functions   
populationRate :: Individual i => [i] -> [(Int, i)]
populationRate population = 
   let evaluates = map fitnesse population
   in zip evaluates population

mutateBitString :: BitStringInd bs => bs -> Int -> bs   
mutateBitString ind locale = setGenome ind $ inverseBit (getGenome ind) locale

crossoverBitString :: BitStringInd bs => (bs, bs) -> Int -> (bs, bs)
crossoverBitString (ind1, ind2) locale =
      let g1 = getGenome ind1
          g2 = getGenome ind2
      in (setGenome ind1 ((highs g1 locale) .|. (low g2 locale)) , setGenome ind2 ((highs g2 locale) .|. (low g1 locale))) 
      
--- util functions
genomes :: BitStringInd i => [i] -> [Int]
genomes = map getGenome

maxFitnesseStop :: Individual i => Int -> [i] -> Bool
maxFitnesseStop mx = any (maxFitnesseReached mx)

maxFitnesseReached :: Individual i => Int -> i -> Bool
maxFitnesseReached mx ind = fitnesse ind == mx

 
 
