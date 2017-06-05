{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
module GenAlg where
import CongruentGenerator
import Data.Bits
import BitOperations
import Data.List
import Utils
import Data.Function

-- Data
data Individual = Individual {genome :: Int, bitLength :: Int}
   deriving Show

data GenAlgContext = GenAlgContext {
   rndContext :: RandomContext,
   mutationProb :: Int,
   crossoverProb :: Int
   } deriving Show

defaultGenAlgContext = GenAlgContext {rndContext = simpleRndContext{token = 100}, 
   mutationProb = 15,
   crossoverProb = 60}
   
-- Functions

--support functions
individualsOf :: Int -> Int -> Int -> [Individual]
individualsOf val bitNum size = expandElems (\x->Individual{genome=val, bitLength=bitNum}) (size-1)

individualsFromTo :: Int -> Int -> Int -> [Individual]
individualsFromTo from to bitNum = [Individual{genome=g, bitLength=bitNum} | g <- [from..to]]

-- GA functions
processGAsingle :: Int -> Int -> GenAlgContext -> (Int, Individual)
processGAsingle bitNum size context = 
   let (count, inds) = processGA bitNum size context
   in (count, head $ sortBy (\ind1 ind2 -> (compare `on` fitnesse) ind2 ind1) inds)

processGA :: Int -> Int -> GenAlgContext -> (Int, [Individual])
processGA bitNum size context = 
   let init = (1, createFirstGeneration bitNum size context)
       iteratedFunc = (\(count, (inds, context)) -> 
                          if onesStop bitNum inds
                          then Nothing
                          else if count > 1000 
                               then Nothing 
                               else let (newInds, ctx) = newGeneration inds context 
                                    in Just ((count,newInds), (count+1, (newInds, ctx))))
   in head $ reverse $ unfoldr iteratedFunc init

-- create intial population of algorithm
createFirstGeneration :: Int -> Int -> GenAlgContext -> ([Individual], GenAlgContext)
createFirstGeneration bitNum size context@GenAlgContext{rndContext} = 
   let upBound = determineUpNum bitNum
       (rands, newContext) = randVector upBound size rndContext
   in ([Individual{genome = r, bitLength = bitNum} | r <- rands], context{rndContext = newContext})

fitnesse :: Individual -> Int
fitnesse ind@Individual {genome = g} 
   | g == 0 = 0
   | otherwise = let digit  = 1 .&. g
      in digit + fitnesse ind {genome = shiftR g 1}      

   
mutate :: Individual -> Int -> Individual
mutate Individual{genome, bitLength} locale = Individual (inverseBit genome locale) bitLength

crossover :: (Individual, Individual) -> Int -> (Individual, Individual)
crossover (ind1@Individual {genome = g1}, ind2@Individual {genome = g2}) locale = 
   (ind1{genome = (high g1 locale) .|. (low g2 locale)}, ind2 {genome = (high g2 locale) .|. (low g1 locale)}) 
   
populationRate :: [Individual] -> [(Int, Individual)]
populationRate population = 
   let evaluates = map fitnesse population
   in zip evaluates population
   
createCrossoverPool :: [Individual] -> RandomContext -> ([(Individual, Individual)], RandomContext)
createCrossoverPool population context = 
   let evals = populationRate population
       (inds1, gen1) =  gainElements evals ((length population) `div` 2) context
       (inds2, gen2) = gainElements evals ((length population) `div` 2) gen1
   in ([(x,y) | x <- inds1 | y <- inds2], gen2)
   
   
newGeneration :: [Individual] -> GenAlgContext -> ([Individual], GenAlgContext)
newGeneration population@(Individual{bitLength = upBound} : _) context@GenAlgContext {rndContext, mutationProb, crossoverProb} = 
   let (pool, g1) = createCrossoverPool population rndContext
       (crossovered, g2) = probableApply (applyOperator crossover upBound)  crossoverProb (pool,g1)
       (mutated, g3) = probableApply (applyOperator mutate upBound) mutationProb ((gatherElems crossovered), g2)
   in (mutated, context{rndContext = g3})
   
onesStop :: Int -> [Individual] -> Bool
onesStop bitLength population = 
   let probableFound = find (==bitLength) $ fst $ unzip $ populationRate population
   in case probableFound of
      Just _ -> True
      otherwise -> False
       

-- private functions
determineUpNum:: Int -> Int
determineUpNum bitNum = powOfTwo (bitNum - 1) + 1 
   
gainElements :: [(Int,a)] -> Int -> RandomContext -> ([a], RandomContext)
gainElements evals num context = 
   let (rands,newContext) = randVector num (evalsSum evals) context
   in ([se | r <- rands, let se = selectElement evals r], newContext)
   
selectRndElement :: [(Int, a)] -> RandomContext -> (a, RandomContext)
selectRndElement population context = 
   let upBound = evalsSum population 
       (dice, newContext) = rand upBound context
   in (selectElement population dice, newContext)
   
selectElement :: [(Int, a)] -> Int -> a
selectElement population num = 
   let (eval, ind) = unzip population
       upBound = sum eval       
   in snd $ head $ dropWhile (\(le, _) -> le <= (num `normalize` (upBound - 1))) $ zip (transformToLadder eval) ind
      where normalize = (\x up -> if x > up then up else x)
   
transformToLadder :: [Int] -> [Int]
transformToLadder = reverse
   .foldr (\val list -> 
      if null list 
      then val:[] 
      else (val + head list):list) []
   .reverse
   
evalsSum :: [(Int,a)] -> Int
evalsSum elems = sum $ fst $ unzip elems

applyOperator :: (a -> Int -> a) -> Int -> ([a], RandomContext) -> ([a], RandomContext)
applyOperator op up (elems, context) =
   let (dices, newContext) = randVector (length elems) (up+1) context
   in ([op x dice | x <- elems | dice <- dices], newContext)
   
probableApply :: (([a], RandomContext) -> ([a], RandomContext)) -> Int -> ([a], RandomContext) -> ([a], RandomContext)
probableApply f prob (elems, context) = 
   let (apply, ignore, g1) = spanWithProb prob (elems, context)
       (result, g2) = f (apply, g1)
   in (result ++ ignore , g2)

filterWithProb :: [a] -> Int -> RandomContext -> ([a], RandomContext)
filterWithProb elems prob context = 
   let (dices, newContext) = randVector (length elems) 100 context
   in  (snd $ unzip $ filter (\(dice, e)-> dice < prob) $ zip dices elems, newContext)
   
spanWithProb :: Int -> ([a], RandomContext) -> ([a],[a],RandomContext)
spanWithProb prob (elems, context) = 
   let (dices, newContext) = randVector (length elems) 100 context
       (zelems1, zelems2) = partition (\(dice,elem) -> dice < prob) $ zip dices elems 
   in (snd $ unzip zelems1, snd $ unzip zelems2, newContext)
