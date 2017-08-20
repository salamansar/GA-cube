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
   
startVertex :: [Edge] -> Vertex
startVertex ((v,_):_) = v

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
      
 
