{-# LANGUAGE TemplateHaskell #-}
module HamiltonGraph where

import Data.Graph
import GaGraph
import Individual
import Control.Monad.State
import Control.Lens
import Control.Monad

data HamiltonGraph = HamiltonGraph {graph :: Graph, genome :: Int}

data HamiltonEdgesState = HamiltonEdgesState {_outbound :: [Vertex], _inbound :: [Vertex]}

makeLenses ''HamiltonEdgesState

instance Individual HamiltonGraph where
    mutate = mutateBitString
    crossover = crossoverBitString
    maxLocale HamiltonGraph{graph = g} = hamiltonPathDimension g - 1
    fitnesse eg@HamiltonGraph{graph = g} = hamiltonFitnesse1 g $ hamiltonPhenotype eg
   
instance BitStringInd HamiltonGraph where
    getGenome ind = genome ind
    setGenome ind newG = ind{genome = newG}
   
hamiltonPathDimension :: Graph -> Int
hamiltonPathDimension g = (vertexDimension g) * (verticesNum g)

hamiltonPhenotype :: HamiltonGraph -> [Vertex]
hamiltonPhenotype HamiltonGraph{graph = g, genome = gen} = mapFromBits gen len cnt
      where len = vertexDimension g
            cnt = verticesNum g 

-- fitnesse is count of unique edges in which vertexes isn't repeated            
hamiltonFitnesse1 :: Graph -> [Vertex] -> Int
hamiltonFitnesse1 g = countHamiltonEdges
     .(formUniqueEdgesSequence g)
    

countHamiltonEdges :: [Edge] -> Int
countHamiltonEdges e = evalState (foldM doCountHamiltonEdges 0 e) initState
   where initState = HamiltonEdgesState {_outbound = [], _inbound = [startVertex e]}
   


doCountHamiltonEdges :: Int -> Edge -> State HamiltonEdgesState Int
doCountHamiltonEdges accum (o,i) = 
   do outbounds <- use outbound
      inbounds <- use inbound
      if (not (o `elem` outbounds)) && (not (i `elem` inbounds)) 
      then do
        outbound .= o:outbounds
        inbound .= i:inbounds
        return (accum + 1) 
      else return accum     
      
-- fitnesse is score = m + (max {|Pe|} - 1) * n,
-- where m - count of unique edges in path
--       |Pe| - cardinality of uniterrupted subpath
--       {|Pe|} - set of cardinalities of all uniterrupted subpaths
--       n -  count of vertexes
hamiltonFitnesse2 :: Graph -> [Vertex] -> Int
hamiltonFitnesse2 g path = m + (maxP - 1) * n
   where m = length $ formUniqueEdgesSequence g path
         maxP = maxCardinality g path
         n = verticesNum g

maxCardinality :: Graph -> [Vertex] -> Int
maxCardinality g = maxDef0
   .(map length)
   .(filter (\p -> (length.uniqueEdges) p == length p))
   .(subpaths g)

maxDef0 :: [Int] -> Int
maxDef0 [] = 0
maxDef0 v = maximum v

subpaths :: Graph -> [Vertex] -> [[Edge]]
subpaths g [] = []
subpaths g (v:[]) = []
subpaths g vertexes@(v:tail) = (splitToSubPaths $ subpath g vertexes) ++ subpaths g tail

splitToSubPaths :: [Edge] -> [[Edge]]
splitToSubPaths path = evalState (foldM doSplitToSubPaths [] path) [] 

doSplitToSubPaths :: [[Edge]] -> Edge -> State [Edge] [[Edge]]
doSplitToSubPaths accum e = 
   do cp <- get
      let np = cp ++ [e]
      put np
      return $ np:accum 
     

subpath :: Graph -> [Vertex] -> [Edge]
subpath g =  (takeWhile (flip hasEdge g)).formEdges
