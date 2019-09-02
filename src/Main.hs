module Main where

import GaGraph
import Data.Graph
import Data.GraphViz
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types
import Data.List
import qualified EilerGenAlg as Ega
import EilerGraph
import qualified GenAlg2 as Ga2
import GenAlgEngine
import GraphSamples

main::IO ()
main = do
   g <- (readDotFile "srcGraph.dot")::IO (DotGraph Int)
   putStrLn "Source graph is:"
   putDot g
   let (mn, mx) = graphBounds g
   let edges = dotGraphEdges mn g
   let inGraph = buildG (0, mx - mn) edges
   let eilerResult = getEilerResult inGraph
   let rg = graphFromNodes mn eilerResult
   putStrLn "Result graph is:\n"
   putDot rg
   runGraphvizCommand Dot rg Png "resultGraph.png"
   return ()

getOnesResult :: String
getOnesResult = show $ Ga2.processOnesGA1 24 16 Ga2.defaultGenAlgContext

getEilerResult :: Graph -> [Int]
getEilerResult g = let (count, result) = runGA Ega.defaultGenAlgContext (Ega.eilerFirstGen g 128) (Ega.eilerPathFound g)
   in case find Ega.isEilerPath result of
      Nothing -> []
      Just eg -> eilerPhenotype eg

graphBounds :: DotGraph Int -> Bounds
graphBounds g = let nodes = map nodeID (graphNodes g)
                in (minimum nodes, maximum nodes)
                
dotGraphEdges :: Int -> DotGraph Int -> [Edge]
dotGraphEdges adjRate = (adjustNodes adjRate)
                        .withBackwards
                        .(map toEdge)
                        .graphEdges
   where toEdge de = (fromNode de, toNode de)
         withBackwards srcEdges = srcEdges ++ (map (\(a,b) -> (b,a)) srcEdges)
         adjustNodes adjRate = map (\(a,b) -> (a - adjRate, b - adjRate))

graphFromNodes :: Int -> [Vertex] -> DotGraph Int
graphFromNodes adjRate v = 
   let adjV = map (+adjRate) v 
   in graphElemsToDot params (createNodes adjV) (createEdges adjV)
      where params = quickParams::GraphvizParams Int Int Int () Int
            createNodes = (map (\x -> (x,x))).nub 
            createEdges vx = zipWith (\(a,b) c -> (a,b,c)) (formEdges vx) [1..]  
