module EilerGraph where

import Data.Graph
import Data.Bits
import Data.List
import BitOperations
import Individual



data EilerGraph = EilerGraph {graph :: Graph, genome :: Int}

instance Individual EilerGraph where
   fitnesse eg@EilerGraph{graph = g} = eilerFitnesse g $ phenotype eg
   mutate = mutateBitString
   crossover = crossoverBitString
   maxLocale EilerGraph{graph = g} = pathDimension g - 1
   
instance BitStringInd EilerGraph where
   getGenome ind = genome ind
   setGenome ind newG = ind{genome = newG}
   
phenotype :: EilerGraph -> [Vertex]
phenotype EilerGraph{graph = g, genome = gen} = mapFromBits gen len cnt
      where len = vertexDimension g
            cnt = edgesNum g + 1 

-- mapping from genome to phenotype
mapFromBits :: Int -> Int -> Int -> [Vertex]
mapFromBits _ _ c | c < 1 = [-1]
mapFromBits bitStr len 1 = [low bitStr len]
mapFromBits bitStr len count = current : mapFromBits (shiftR bitStr len) len (count-1)
   where current = low bitStr len
   
-- fitnesse function
eilerFitnesse :: Graph -> [Vertex] -> Int
eilerFitnesse g = foldr (\e -> (hasEdgeN e g +)) 0 
   . uniqueEdges
   . formEdges  

-- help functions
formEdges :: [Vertex] -> [Edge]
formEdges [] = []
formEdges vl@(_:vs) = zip vl vs

hasEdge :: Edge -> Graph -> Bool
hasEdge e = any (== e) .edges

hasEdgeN :: Edge -> Graph -> Int
hasEdgeN e g = if hasEdge e g then 1 else 0

uniqueEdges :: [Edge] -> [Edge]
uniqueEdges = nubBy (\(a1,b1) (a2,b2) -> a1 == a2 && b1 == b2 || a1 == b2 && b1 == a2) -- filter out duplications 
   . filter (\(a,b) -> a /= b) -- filter out selfjoined edges (1,1)

edgesNum :: Graph -> Int
edgesNum = length . uniqueEdges . edges

vertexDimension :: Graph -> Int
vertexDimension =  minDimension.(flip (-) 1).length.vertices

pathDimension :: Graph -> Int
pathDimension g = (vertexDimension g) * (edgesNum g + 1)


