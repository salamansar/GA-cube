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
import qualified BitStringIndRunner as BSR



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

runEilerGaFine :: Graph -> Int -> (Int,[Int])
runEilerGaFine g pop = let (count, result) = runEilerGa g pop
   in (count, map (+1) result)

runEilerGa :: Graph -> Int -> (Int,[Int])
runEilerGa g popSize = let (count, result) = runGA defaultGenAlgContext (eilerFirstGen g popSize) (eilerPathFound g)
   in (count, fetchResult result)
   
runEilerGa2 :: Graph -> Int -> (Int,[Int])
runEilerGa2 g popSize = let (count, result) = BSR.processGAPop popSize (maxEilerFitnesse g) (pathDimension g) (eilerInd g)
   in (count, fetchResult result)
   
   
runEilerGaGen :: Graph -> Int -> (Int,[Int])
runEilerGaGen g popSize = let (count, result) = runGA defaultGenAlgContext (eilerFirstGen g popSize) (eilerPathFound g)
   in (count, map genome result)
      
runEilerGaPhen :: Graph -> Int -> (Int,[[Int]])
runEilerGaPhen g popSize = let (count, result) = runGA defaultGenAlgContext (eilerFirstGen g popSize) (eilerPathFound g)
   in (count, map ((map (+1)).eilerPhenotype) result)
   
runEilerGaFit :: Graph -> Int -> (Int,[Int])
runEilerGaFit g popSize = let (count, result) = runGA defaultGenAlgContext (eilerFirstGen g popSize) (eilerPathFound g)
   in (count, map fitnesse result)

fetchResult :: [EilerGraph] -> [Int]
fetchResult result = case find isEilerPath result of
      Nothing -> []
      Just eg -> eilerPhenotype eg

---factory func
eilerFirstGen :: Graph -> Int -> GenAlgContext -> ([EilerGraph], GenAlgContext)
eilerFirstGen g count ctx = 
    let (rands, newCtx) = randVector count (powOfTwo $ pathDimension g)  $ ctx^.rndContext
    in (eilerInds g rands, ctx&rndContext.~newCtx)

---stop function
eilerPathFound :: Graph -> [EilerGraph] -> Bool
eilerPathFound g = maxFitnesseStop $  maxEilerFitnesse g

maxEilerFitnesse :: Graph -> Int
maxEilerFitnesse = edgesNum

isEilerPath :: EilerGraph -> Bool
isEilerPath eg = fitnesse eg == (maxEilerFitnesse  $ graph eg)

---help functions
eilerInds :: Graph -> [Int] -> [EilerGraph]
eilerInds g = map (eilerInd g)

eilerInd :: Graph -> Int -> EilerGraph
eilerInd g gn = EilerGraph{graph = g, genome = gn}
