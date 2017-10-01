module BitStringIndRunner where

import CongruentGenerator
import Control.Lens
import Control.Monad.State
import Data.List
import GenAlgEngine
import Individual

defaultGenAlgContext = GenAlgContext {_rndContext = simpleRndContext 100, 
   _mutationProb = 15,
   _crossoverProb = 60,
   _maxCount = 100000,
   _count = 0}

--- Invariants of running functions   
processGAPop :: Individual i => Int -> Int -> Int -> (Int -> i) -> (Int, [i])
processGAPop popSize maxLocale maxFit factoryFunc =
   runGAS defaultGenAlgContext (randomGenomeInit popSize maxLocale factoryFunc) (maxFitnesseStop maxFit)
   
processGAInd :: Individual i => Int -> Int -> Int -> (Int -> i) -> (Int, Maybe i)
processGAInd popSize maxLocale maxFit factoryFunc =  
   let (cnt, pop) =  processGAPop popSize maxLocale maxFit factoryFunc
   in (cnt, find (maxFitnesseReached maxFit) pop)

processGAPhen :: Individual i => Int -> Int -> Int -> (Int -> i) -> (i -> a) -> (Int, Maybe a)
processGAPhen popSize maxLocale maxFit factoryFunc phenotypeFunc = 
   let (cnt, ind) = processGAInd popSize maxLocale maxFit factoryFunc
       phen = case ind of 
          Nothing -> Nothing
          Just ind -> Just $ phenotypeFunc ind 
   in (cnt, phen)
   
----
randomGenomeInit :: Individual i => Int -> Int -> (Int -> i) -> State GenAlgContext [i]
randomGenomeInit popSize maxLocale factoryFunc = 
   do zoom rndContext $
         do genomes <- randVectorS popSize maxLocale
            let initInds = do dice <- genomes
                              return $ factoryFunc dice
            return initInds
