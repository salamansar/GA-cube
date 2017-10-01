{-# LANGUAGE LambdaCase #-}
module HamiltonGenAlg where

import BitStringIndRunner
import CongruentGenerator
import Control.Lens
import Data.Graph
import GaGraph
import GenAlgEngine
import HamiltonGraph
import Individual

   
---hamilton GA main functions
runHamiltonGaFine :: Graph -> Int -> (Int, Maybe [Int])
runHamiltonGaFine g popSize = 
   let (cnt, res) = runHamiltonGa g popSize 
   in (cnt, incrementResult(res))
   where incrementResult = \case Nothing -> Nothing
                                 Just res -> Just $ map (+1) res

runHamiltonGa :: Graph -> Int -> (Int, Maybe [Int])
runHamiltonGa g popSize = processGAPhen popSize (hamiltonPathDimension g) (maxFitnesse g) (hamiltonInd g) hamiltonPhenotype

-- private functions
maxFitnesse :: Graph -> Int
maxFitnesse g = n * (n - 1) -1
   where n = verticesNum g

hamiltonInd :: Graph -> Int -> HamiltonGraph
hamiltonInd g gn = HamiltonGraph{graph = g, genome = gn}
