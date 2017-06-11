{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module GenAlgEngine where

import Individual
import CongruentGenerator
import OnesInd
import Data.List
import Utils
import Control.Lens
import Control.Monad.State

data GenAlgContext = GenAlgContext {
   _rndContext :: RandomContext,
   _mutationProb :: Int,
   _crossoverProb :: Int,
   _maxCount :: Int,
   _count :: Int
   } deriving Show
   
makeLenses ''GenAlgContext
   
--Main functions
runGA :: Individual i => GenAlgContext -> (GenAlgContext -> ([i], GenAlgContext)) -> ([i] -> Bool) -> (Int, [i])
runGA initContext factoryFunc stopFunc = 
   let (count,finalPop,_) = runGADetail initContext factoryFunc stopFunc
   in (count, finalPop)

runGADetail :: Individual i => GenAlgContext -> (GenAlgContext -> ([i], GenAlgContext)) -> ([i] -> Bool) -> (Int, [i], GenAlgContext)
runGADetail initContext factoryFunc stopFunc = 
   let (initPop,context) = factoryFunc initContext 
       (finalPop,finalContext) = runState (stepOnGA initPop stopFunc) $ context&count.~0
       in (finalContext^.count, finalPop, finalContext)

--------------
      
stepOnGA :: Individual i => [i] -> ([i] -> Bool) -> State GenAlgContext [i]
stepOnGA inds stopFunc = 
   do count += 1
      cnt <- use count      
      maxCnt <- use maxCount
      if (stopFunc inds) || (cnt > maxCnt)
         then return inds
         else do newInds <- newGeneration inds
                 stepOnGA newInds stopFunc
   
newGeneration :: Individual i => [i] -> State GenAlgContext [i]
newGeneration population@(ind : _) = 
   do crossProb <- use crossoverProb
      mutateProb <- use mutationProb
      let max = maxLocale ind
      let crossoverOp = applyOperator crossover max
      let mutationOp = applyOperator mutate max
      zoom rndContext $
         do pool <- createCrossoverPool population                  
            crossovered <- probableApply crossoverOp crossProb pool -- crossover
            probableApply mutationOp mutateProb (gatherElems crossovered) -- mutation
   
-- **** private functions ***** ---

-- crossover pool   
createCrossoverPool :: Individual i => [i] -> State RandomContext [(i, i)]
createCrossoverPool population = 
   do let evals = populationRate population
      inds1 <- gainElements evals ((length population) `div` 2)
      inds2 <- gainElements evals ((length population) `div` 2)
      return [(x,y) | x <- inds1 | y <- inds2]
   
gainElements :: [(Int,a)] -> Int -> State RandomContext [a]
gainElements evals num = 
   do rands <- randVectorS num (evalsSum evals)
      return [se | r <- rands, let se = selectElement evals r]
   
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

-- apply operators   
probableApply :: ([a] -> State RandomContext [a]) -> Int -> [a] -> State RandomContext [a]
probableApply f prob elems = 
   do (apply, ignore) <- spanWithProb prob elems
      result <- f apply
      return (result ++ ignore)

spanWithProb :: Int -> [a] -> State RandomContext ([a],[a])
spanWithProb prob elems = 
   do dices <-  randVectorS (length elems) 100
      let (zelems1, zelems2) = partition (\(dice,elem) -> dice < prob) $ zip dices elems 
      return (snd $ unzip zelems1, snd $ unzip zelems2)
   
applyOperator :: (a -> Int -> a) -> Int -> [a] -> State RandomContext [a]
applyOperator op up elems =
   do dices <- randVectorS (length elems) (up+1)
      return [op x dice | x <- elems | dice <- dices]



