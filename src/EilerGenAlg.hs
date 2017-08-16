module EilerGenAlg where

import BitOperations
import CongruentGenerator
import Control.Lens
import Data.Graph
import Data.List
import EilerGraph
import GenAlgEngine
import Individual
import GaGraph



defaultGenAlgContext = GenAlgContext {_rndContext = simpleRndContext 100, 
   _mutationProb = 15,
   _crossoverProb = 60,
   _maxCount = 100000,
   _count = 0}

defaultGenAlgContext2 :: Int -> GenAlgContext
defaultGenAlgContext2 token = GenAlgContext {_rndContext = simpleRndContext token, 
   _mutationProb = 15,
   _crossoverProb = 60,
   _maxCount = 5000,
   _count = 0}

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
   
runEilerGaGen :: Graph -> Int -> (Int,[Int])
runEilerGaGen g popSize = let (count, result) = runGA defaultGenAlgContext (eilerFirstGen g popSize) eilerPathFound
   in (count, map genome result)
      
runEilerGaPhen :: Graph -> Int -> (Int,[[Int]])
runEilerGaPhen g popSize = let (count, result) = runGA defaultGenAlgContext (eilerFirstGen g popSize) eilerPathFound
   in (count, map ((map (+1)).phenotype) result)
   
runEilerGaFit :: Graph -> Int -> (Int,[Int])
runEilerGaFit g popSize = let (count, result) = runGA defaultGenAlgContext (eilerFirstGen g popSize) eilerPathFound
   in (count, map fitnesse result)

fetchResult :: [EilerGraph] -> [Int]
fetchResult result = case find isEilerPath result of
      Nothing -> []
      Just eg -> phenotype eg

---factory func
eilerFirstGen :: Graph -> Int -> GenAlgContext -> ([EilerGraph], GenAlgContext)
eilerFirstGen g count ctx = 
    let (rands, newCtx) = randVector count (powOfTwo $ pathDimension g)  $ ctx^.rndContext
    in (eilerInds g rands, ctx&rndContext.~newCtx)


---stop function
eilerPathFound :: [EilerGraph] -> Bool
eilerPathFound = any isEilerPath

isEilerPath :: EilerGraph -> Bool
isEilerPath eg = fitnesse eg == (edgesNum $ graph eg)

---help functions
eilerInds :: Graph -> [Int] -> [EilerGraph]
eilerInds g = map (\gn -> EilerGraph{graph = g, genome = gn})
