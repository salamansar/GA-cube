module GaGraph where

import Data.Graph
import BitOperations
import Data.Bits
import Data.List

-- mapping from genome to vertexes
mapFromBits :: Int -> Int -> Int -> [Vertex]
mapFromBits _ _ c | c < 1 = [-1]
mapFromBits bitStr len 1 = [lows bitStr len]
mapFromBits bitStr len count = current : mapFromBits (shiftR bitStr len) len (count-1)
   where current = lows bitStr len
   
formEdges :: [Vertex] -> [Edge]
formEdges [] = []
formEdges vl@(_:vs) = zip vl vs

hasEdge :: Edge -> Graph -> Bool
hasEdge e = any (== e) .edges

hasEdgeN :: Edge -> Graph -> Int
hasEdgeN e g = if hasEdge e g then 1 else 0

existingOnlyEdges :: Graph -> [Edge] -> [Edge]
existingOnlyEdges g = filter (flip hasEdge g)

uniqueEdges :: [Edge] -> [Edge]
uniqueEdges = nubBy (\(a1,b1) (a2,b2) -> a1 == a2 && b1 == b2 || a1 == b2 && b1 == a2) -- filter out duplications 
   .filter (\(a,b) -> a /= b) -- filter out selfjoined edges (1,1)
   
formUniqueEdgesSequence :: Graph -> [Vertex] -> [Edge]
formUniqueEdgesSequence g = (existingOnlyEdges g)
   .uniqueEdges
   .formEdges

edgesNum :: Graph -> Int
edgesNum = length . uniqueEdges . edges

verticesNum :: Graph -> Int
verticesNum = length . vertices

vertexDimension :: Graph -> Int
vertexDimension =  minDimension.(flip (-) 1).length.vertices
