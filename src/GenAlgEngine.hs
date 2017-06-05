{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NamedFieldPuns #-}
module GenAlgEngine where

import Individual
import CongruentGenerator
import OnesInd
import Data.List
import Utils

data GenAlgContext = GenAlgContext {
   rndContext :: RandomContext,
   mutationProb :: Int,
   crossoverProb :: Int,
   maxCount :: Int
   } deriving Show
   
--Main functions
runGA :: Individual i => GenAlgContext -> (GenAlgContext -> ([i], GenAlgContext)) -> ([i] -> Bool) -> (Int, [i])
runGA initContext factoryFunc stopFunc = stepOnGA (1, (factoryFunc initContext)) stopFunc

runGADetail :: Individual i => GenAlgContext -> (GenAlgContext -> ([i], GenAlgContext)) -> ([i] -> Bool) -> (Int, [i], GenAlgContext)
runGADetail initContext factoryFunc stopFunc = stepOnGADetail (0, (factoryFunc initContext)) stopFunc
   
stepOnGA :: Individual i => (Int, ([i], GenAlgContext)) -> ([i] -> Bool) -> (Int, [i])
stepOnGA (count, (inds, context)) stopFunc = 
   if (stopFunc inds) || (count > (maxCount context))
   then (count, inds)
   else let (newInds, ctx) = newGeneration inds context 
      in stepOnGA (count+1, (newInds, ctx)) stopFunc
      
stepOnGADetail :: Individual i => (Int, ([i], GenAlgContext)) -> ([i] -> Bool) -> (Int, [i], GenAlgContext)
stepOnGADetail (count, (inds, context)) stopFunc = 
   if (stopFunc inds) || (count >= (maxCount context))
   then (count, inds, context)
   else let (newInds, ctx) = newGeneration inds context 
      in stepOnGADetail (count+1, (newInds, ctx)) stopFunc
   
newGeneration :: Individual i => [i] -> GenAlgContext -> ([i], GenAlgContext)
newGeneration population@(ind : _) context@GenAlgContext {rndContext, mutationProb, crossoverProb} = 
   let (pool, g1) = createCrossoverPool population rndContext
       (crossovered, g2) = probableApply (applyOperator crossover $ maxLocale ind)  crossoverProb (pool,g1)
       (mutated, g3) = probableApply (applyOperator mutate $ maxLocale ind) mutationProb ((gatherElems crossovered), g2)
   in (mutated, context{rndContext = g3})
   
--private functions
createCrossoverPool :: Individual i =>[i] -> RandomContext -> ([(i, i)], RandomContext)
createCrossoverPool population context = 
   let evals = populationRate population
       (inds1, gen1) =  gainElements evals ((length population) `div` 2) context
       (inds2, gen2) = gainElements evals ((length population) `div` 2) gen1
   in ([(x,y) | x <- inds1 | y <- inds2], gen2)   


gainElements :: [(Int,a)] -> Int -> RandomContext -> ([a], RandomContext)
gainElements evals num context = 
   let (rands,newContext) = randVector num (evalsSum evals) context
   in ([se | r <- rands, let se = selectElement evals r], newContext)
   
evalsSum :: [(Int,a)] -> Int
evalsSum elems = sum $ fst $ unzip elems

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
   
probableApply :: (([a], RandomContext) -> ([a], RandomContext)) -> Int -> ([a], RandomContext) -> ([a], RandomContext)
probableApply f prob (elems, context) = 
   let (apply, ignore, g1) = spanWithProb prob (elems, context)
       (result, g2) = f (apply, g1)
   in (result ++ ignore , g2)
   
spanWithProb :: Int -> ([a], RandomContext) -> ([a],[a],RandomContext)
spanWithProb prob (elems, context) = 
   let (dices, newContext) = randVector (length elems) 100 context
       (zelems1, zelems2) = partition (\(dice,elem) -> dice < prob) $ zip dices elems 
   in (snd $ unzip zelems1, snd $ unzip zelems2, newContext)
   
applyOperator :: (a -> Int -> a) -> Int -> ([a], RandomContext) -> ([a], RandomContext)
applyOperator op up (elems, context) =
   let (dices, newContext) = randVector (length elems) (up+1) context
   in ([op x dice | x <- elems | dice <- dices], newContext)



