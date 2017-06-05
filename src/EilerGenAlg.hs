module EilerGenAlg where

import EilerGraph
import GenAlgEngine
import Individual
import CongruentGenerator
import Data.Graph
import BitOperations
import Data.List

defaultGenAlgContext = GenAlgContext {rndContext = simpleRndContext 100, 
   mutationProb = 15,
   crossoverProb = 60,
   maxCount = 5000}

defaultGenAlgContext2 :: Int -> GenAlgContext
defaultGenAlgContext2 token = GenAlgContext {rndContext = simpleRndContext token, 
   mutationProb = 15,
   crossoverProb = 60,
   maxCount = 5000}

testGraph :: Graph
testGraph = buildG (0,3) [(0,1), (1,0), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2)]

testGraph2 :: Graph
testGraph2 = buildG (0,6) [(0,1), (1,0),(1,5), (2,5),(2,3), (3,2),(3,5), (4,5),(4,6), (5,1),(5,2),(5,3),(5,4), (6,4)]

testGraph3 :: Graph
testGraph3 = buildG (0,4) [(0,1),(0,2),(0,3), (1,0),(1,2),(1,4), (2,0),(2,1), (3,0),(3,4), (4,1), (4,3)]

runEilerGaFine :: Graph -> Int -> (Int,[Int])
runEilerGaFine g pop = let (count, result) = runEilerGa g pop
   in (count, map (+1) result)

runEilerGa :: Graph -> Int -> (Int,[Int])
runEilerGa g popSize = let (count, result) = runGA defaultGenAlgContext (eilerFirstGen g popSize) eilerPathFound
   in (count, fetchResult result)
      
fetchResult :: [EilerGraph] -> [Int]
fetchResult result = case find isEilerPath result of
      Nothing -> []
      Just eg -> phenotype eg

---factory func
eilerFirstGen :: Graph -> Int -> GenAlgContext -> ([EilerGraph], GenAlgContext)
eilerFirstGen g count ctx@GenAlgContext{rndContext = rCtx} = 
    let (rands, newCtx) = randVector count (powOfTwo $ pathDimension g)  rCtx
    in (eilerInds g rands, ctx{rndContext = newCtx})


---stop function
eilerPathFound :: [EilerGraph] -> Bool
eilerPathFound = any isEilerPath

isEilerPath :: EilerGraph -> Bool
isEilerPath eg = fitnesse eg == (edgesNum $ graph eg)

---help functions
eilerInds :: Graph -> [Int] -> [EilerGraph]
eilerInds g = map (\gn -> EilerGraph{graph = g, genome = gn})