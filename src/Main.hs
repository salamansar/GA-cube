module Main where

import GaGraph
import Data.Graph
import Data.List
import qualified EilerGenAlg as Ega
import EilerGraph
import qualified GenAlg2 as Ga2
import GenAlgEngine
import GraphSamples
import GraphIO

main::IO ()
main = do
   (srcPath, destPath) <- readParamsInteractive
   (g, adj) <- readGraph srcPath
   let eilerResult = getEilerResult g
   writeVertexes destPath adj eilerResult
   return ()
   
readParamsInteractive :: IO (String, String)
readParamsInteractive = do 
   putStr "Input graph file: "
   srcPath <- getLine
   putStr "Result image file: "
   destPath <- getLine
   return (srcPath, destPath)

getOnesResult :: String
getOnesResult = show $ Ga2.processOnesGA1 24 16 Ga2.defaultGenAlgContext

getEilerResult :: Graph -> [Int]
getEilerResult g = let (count, result) = runGA Ega.defaultGenAlgContext (Ega.eilerFirstGen g 128) (Ega.eilerPathFound g)
   in case find Ega.isEilerPath result of
      Nothing -> []
      Just eg -> eilerPhenotype eg 
