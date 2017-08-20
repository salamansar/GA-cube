module EilerGraph where

import Data.Graph
import Data.Bits
import Data.List
import BitOperations
import Individual
import GaGraph



data EilerGraph = EilerGraph {graph :: Graph, genome :: Int}

instance Individual EilerGraph where
   fitnesse eg@EilerGraph{graph = g} = eilerFitnesse g $ eilerPhenotype eg
   mutate = mutateBitString
   crossover = crossoverBitString
   maxLocale EilerGraph{graph = g} = pathDimension g - 1
   
instance BitStringInd EilerGraph where
   getGenome ind = genome ind
   setGenome ind newG = ind{genome = newG}
   
eilerPhenotype :: EilerGraph -> [Vertex]
eilerPhenotype EilerGraph{graph = g, genome = gen} = mapFromBits gen len cnt
      where len = vertexDimension g
            cnt = edgesNum g + 1 


   
-- fitnesse function
eilerFitnesse :: Graph -> [Vertex] -> Int
eilerFitnesse g = foldr (\e -> (hasEdgeN e g +)) 0 
   . uniqueEdges
   . formEdges  

-- help functions
pathDimension :: Graph -> Int
pathDimension g = (vertexDimension g) * (edgesNum g + 1)


