module Main where

import qualified GenAlg2 as Ga2
import qualified EilerGenAlg as Ega
import GenAlgEngine
import Data.List
import EilerGraph
import GraphSamples

main::IO ()
main = putStrLn $ "Result is: " ++ getOnesResult

getOnesResult :: String
getOnesResult = show $ Ga2.processOnesGA1 24 16 Ga2.defaultGenAlgContext

getEilerResult :: [Int]
getEilerResult = let (count, result) = runGA Ega.defaultGenAlgContext (Ega.eilerFirstGen eilerGraph1 16) Ega.eilerPathFound
   in case find Ega.isEilerPath result of
      Nothing -> []
      Just eg -> eilerPhenotype eg
