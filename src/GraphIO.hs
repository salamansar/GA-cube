module GraphIO where

import Data.Graph
import Data.GraphViz
import Data.GraphViz.Commands.IO
import Data.List
import GaGraph


-- Reading Graph from .dot file
readGraph :: String -> IO (Graph, Int)
readGraph path = do
   g <- (readDotFile path)::IO (DotGraph Int)
   putStrLn "Read graph is:"
   putDot g
   return $ parseFromDot g

-- Transforms DotGraph into Graph with adjust rate   
parseFromDot :: DotGraph Int -> (Graph, Int)
parseFromDot g = let (mn, mx) = graphBounds g
                     edges = dotGraphEdges mn g
                     inGraph = buildG (0, mx - mn) edges
                 in (inGraph, mn)
   
-- Write vertexes into PNG image file with adjust rate
writeVertexes :: String -> Int -> [Vertex] -> IO ()
writeVertexes path adjustRate vertexes = do
   let dg = graphFromNodes adjustRate vertexes
   putStrLn "Written graph is:\n"
   putDot dg
   exportToPng dg path
   
exportToPng :: DotGraph Int -> String -> IO ()
exportToPng dg path = do runGraphvizCommand Dot dg Png path
                         return ()


--------- helper functions------------
   
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
