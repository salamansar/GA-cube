{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE FlexibleContexts #-}
module Main where

import Control.Exception.Base
import Control.Monad.Except
import GaGraph
import Data.Graph
import Data.List
import Data.Maybe
import qualified EilerGenAlg as Ega
import EilerGraph
import qualified GenAlg2 as Ga2
import GenAlgEngine
import GraphSamples
import GraphIO
import System.Console.GetOpt
import System.Environment

type GraphArgs = (String, String)
data CmdArgs = CmdArgs {
   graphArgs::Maybe GraphArgs,
   isInteractive::Bool,
   isHelpRequested::Bool
} deriving Show

emptyArgs = CmdArgs { 
   graphArgs = Nothing,
   isInteractive = False,
   isHelpRequested = False
   }

main::IO ()
main = do 
   args <- getArgs
   case readCmdArgs args of
      Right parsedArgs -> handleCmdArgs parsedArgs
      Left e -> putStrLn e
   return ()

readCmdArgs :: [String] -> Either String CmdArgs
readCmdArgs args = do
   cmdArgs <- parseCmdArgs args
   case cmdArgs of
      CmdArgs { isHelpRequested = True, graphArgs = Nothing, isInteractive = False } -> Right cmdArgs
      CmdArgs { isHelpRequested = False, graphArgs = Just (i,o), isInteractive = False } | (i /= "") && (o /= "") -> Right cmdArgs
      CmdArgs { isHelpRequested = False, graphArgs = Nothing, isInteractive = True } -> Right cmdArgs
      otherwise -> throwError "Wrong combination of input arguments"

parseCmdArgs :: MonadError String m => [String] -> m CmdArgs
parseCmdArgs [] = return emptyArgs { isInteractive = True }
parseCmdArgs args = case getOpt Permute cmdOptions args of
      (optsF, [], []) -> return $ foldr id emptyArgs optsF
      (_, _, e) -> throwError $ "Error in arguments parsing. " ++ (concat e)
      
cmdOptions :: [OptDescr (CmdArgs -> CmdArgs)]
cmdOptions = [
                Option ['i'] ["input"] (OptArg (parseIO fillInput) "INPUT") "Input graph .dot filename",
                Option ['o'] ["output"] (OptArg (parseIO fillOutput) "OUTPUT") "Output PNG filename",
                Option ['h'] ["help"] (NoArg (\opts -> opts { isHelpRequested = True })) "Print help"
             ]
   where parseIO f p opts = maybe opts (f opts) p
         fillInput opts@CmdArgs{graphArgs=ga} p = case ga of 
                                                     Just (i,o) -> opts { graphArgs = Just (p,o) }
                                                     Nothing -> opts { graphArgs = Just (p, "") }
         fillOutput opts@CmdArgs{graphArgs=ga} p = case ga of 
                                                     Just (i,o) -> opts { graphArgs = Just (i,p) }
                                                     Nothing -> opts { graphArgs = Just ("", p) }
      
handleCmdArgs :: CmdArgs -> IO()
handleCmdArgs args = do 
   case args of
      CmdArgs { isHelpRequested = True } -> putStrLn getUsageInfo
      otherwise -> handleGaArgs args
                                            
handleGaArgs :: CmdArgs -> IO()
handleGaArgs args = do 
   (srcPath, destPath) <- case args of
      CmdArgs { isInteractive = True } -> readGraphParamsInteractive
      CmdArgs { graphArgs = Just (i,o) } -> return (i, o)
   (g, adj) <- readGraph srcPath
   case getEilerResult g of
      Left e -> putStrLn e
      Right result -> do writeVertexes destPath adj result
                         putStrLn $ "Result was written into " ++ destPath
 
getUsageInfo :: String
getUsageInfo = usageInfo "Usage: \n\tEmpty args - interactive mode\n\tParams from cmd: -i[INPUT] -o[OUTPUT]\n\tHelp: -h" cmdOptions                                           

readGraphParamsInteractive :: IO GraphArgs
readGraphParamsInteractive = do 
   putStr "Input graph file: "
   srcPath <- getLine
   putStr "Result image file: "
   destPath <- getLine
   return (srcPath, destPath)
   
getOnesResult :: String
getOnesResult = show $ Ga2.processOnesGA1 24 16 Ga2.defaultGenAlgContext

getEilerResult :: Graph -> Either String [Int]
getEilerResult g = let (count, result) = runGA Ega.defaultGenAlgContext (Ega.eilerFirstGen g 128) (Ega.eilerPathFound g)
   in case find Ega.isEilerPath result of
      Nothing -> throwError "Solution wasn't found"
      Just eg -> return $ eilerPhenotype eg 
