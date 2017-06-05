{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
module GenAlg2 where
import CongruentGenerator
import Data.List
import GenAlgEngine
import Individual
import OnesInd
import Utils
import Data.Function
import BitOperations

defaultGenAlgContext = GenAlgContext {rndContext = simpleRndContext 100, 
   mutationProb = 15,
   crossoverProb = 60,
   maxCount = 500000}
   
-- Functions

--support functions
individualsOf :: Int -> Int -> Int -> [BSInd]
individualsOf val bitNum size = expandElems (\x->OnesInd{genome=val, bitLength=bitNum}) (size-1)

individualsFromTo :: Int -> Int -> Int -> [BSInd]
individualsFromTo from to bitNum = [OnesInd{genome=g, bitLength=bitNum} | g <- [from..to]]

-- GA functions
processSingle :: ((Int, [BSInd])) -> (Int, BSInd)
processSingle pf = 
   let (count, inds) = pf
   in (count, head $ sortBy (\ind1 ind2 -> (compare `on` fitnesse) ind2 ind1) inds)

processOnesGA1 :: Int -> Int -> GenAlgContext -> (Int, [BSInd])
processOnesGA1 bitNum size context =  runGA context (maxOnesExcludeFirstGen bitNum size createOnesInd) (maxStop bitNum)

processOnesGA2 :: Int -> Int -> GenAlgContext -> (Int, [BSInd])
processOnesGA2 bitNum size context =  runGA context (minZerosExcludeFirstGen bitNum size createOnesInd) (maxStop bitNum)

processOnesGA3 :: Int -> Int -> GenAlgContext -> (Int, [BSInd])
processOnesGA3 bitNum size context =  runGA context (zerosFirstGen bitNum size createOnesInd) (maxStop bitNum)

processOnesGA4 :: Int -> Int -> GenAlgContext -> (Int, [BSInd])
processOnesGA4 bitNum size context =  runGA context (onesFirstGen bitNum size createOnesInd) (maxStop bitNum)

processZerosGA1 :: Int -> Int -> GenAlgContext -> (Int, [BSInd])
processZerosGA1 bitNum size context =  runGA context (maxOnesExcludeFirstGen bitNum size createZerosInd) (maxStop bitNum)

processZerosGA2 :: Int -> Int -> GenAlgContext -> (Int, [BSInd])
processZerosGA2 bitNum size context =  runGA context (minZerosExcludeFirstGen bitNum size createZerosInd) (maxStop bitNum)

processZerosGA3 :: Int -> Int -> GenAlgContext -> (Int, [BSInd])
processZerosGA3 bitNum size context =  runGA context (zerosFirstGen bitNum size createZerosInd) (maxStop bitNum)

processZerosGA4 :: Int -> Int -> GenAlgContext -> (Int, [BSInd])
processZerosGA4 bitNum size context =  runGA context (onesFirstGen bitNum size createZerosInd) (maxStop bitNum)

-- create intial population of algorithm
maxOnesExcludeFirstGen :: Int -> Int -> (Int -> Int -> BSInd) -> GenAlgContext -> ([BSInd], GenAlgContext)
maxOnesExcludeFirstGen bitNum size createFunc context@GenAlgContext{rndContext} = 
   let upBound = determineUpNum bitNum
       (rands, newContext) = randVector size upBound rndContext
   in ([createFunc r bitNum | r <- rands], context{rndContext = newContext})
   
minZerosExcludeFirstGen :: Int -> Int -> (Int -> Int -> BSInd) -> GenAlgContext -> ([BSInd], GenAlgContext)
minZerosExcludeFirstGen bitNum size createFunc context@GenAlgContext{rndContext} = 
   let upBound = powOfTwo bitNum - 2
       (rands, newContext) = randVector size upBound rndContext
   in ([createFunc (r + 1) bitNum | r <- rands], context{rndContext = newContext})
   
zerosFirstGen :: Int -> Int -> (Int -> Int -> BSInd) -> GenAlgContext -> ([BSInd], GenAlgContext)
zerosFirstGen bitNum size createFunc context = ([createFunc 0 bitNum | count <- [1..size]], context)
   
onesFirstGen :: Int -> Int -> (Int -> Int -> BSInd) -> GenAlgContext -> ([BSInd], GenAlgContext)
onesFirstGen bitNum size createFunc context = ([createFunc (powOfTwo bitNum - 1) bitNum | count <- [1..size]], context)


 
--stop functions
maxStop :: Individual i => Int -> [i] -> Bool
maxStop bitLength population = stopFunc population (==bitLength)
      
minStop :: Individual i => Int -> [i] -> Bool
minStop bitLength population = stopFunc population (==0)
       
stopFunc :: Individual i => [i] -> (Int -> Bool) -> Bool 
stopFunc population compareFunc = 
   let probableFound = find compareFunc $ fst $ unzip $ populationRate population
   in case probableFound of
      Just _ -> True
      otherwise -> False

-- private functions
determineUpNum:: Int -> Int
determineUpNum bitNum = powOfTwo (bitNum - 1) - 1 
