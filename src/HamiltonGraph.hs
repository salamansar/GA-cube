module HamiltonGraph where

import Data.Graph
import GaGraph
import Individual

data HamiltonGraph = HamiltonGraph {graph :: Graph, genome :: Int}

-- instance Individual HamiltonGraph where
   -- mutate = mutateBitString
   -- crossover = crossoverBitString
   -- maxLocale EilerGraph{graph = g} = pathDimension g - 1
   -- fitnesse eg@EilerGraph{graph = g} = eilerFitnesse g $ phenotype eg
   
-- instance BitStringInd HamiltonGraph where
   -- getGenome ind = genome ind
   -- setGenome ind newG = ind{genome = newG}
 
