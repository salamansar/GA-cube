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
import GraphIO
import System.Console.GetOpt
import System.Environment
import EilerServer
import qualified Data.Text.Lazy as T
import Data.GraphViz

type GraphArgs = (String, String)
type ServerArgs = (String, Int)
data CmdArgs = CmdArgs {
   graphArgs::Maybe GraphArgs,
   isInteractive::Bool,
   isHelpRequested::Bool,
   isServerMode::Bool,
   isClientMode::Bool,
   serverArgs::Maybe ServerArgs
} deriving Show

emptyArgs = CmdArgs { 
   graphArgs = Nothing,
   isInteractive = False,
   isHelpRequested = False,
   isServerMode = False,
   isClientMode = False,
   serverArgs = Nothing
   }

main::IO ()
main = do 
   args <- getArgs
   case readCmdArgs args of
      Right parsedArgs -> handleCmdArgs parsedArgs
      Left e -> putStrLn e

readCmdArgs :: [String] -> Either String CmdArgs
readCmdArgs args = do
   cmdArgs <- parseCmdArgs args
   case cmdArgs of
      CmdArgs { isHelpRequested = True, 
                graphArgs = Nothing, 
                isInteractive = False, 
                isClientMode = False, 
                isServerMode = False, 
                serverArgs = Nothing 
              } -> return cmdArgs
      CmdArgs { isHelpRequested = False, 
                graphArgs = Just (i,o), 
                isInteractive = False,
                isClientMode = False, 
                isServerMode = False, 
                serverArgs = Nothing 
              } | (i /= "") && (o /= "") -> return cmdArgs
      CmdArgs { isHelpRequested = False, 
                graphArgs = Just (i,o), 
                isInteractive = False,
                isClientMode = True, 
                isServerMode = False, 
                serverArgs = Just (h,p) 
              } | (h /= "") && (p /= 0) && (i /= "") && (o /= "") -> return cmdArgs
      CmdArgs { isHelpRequested = False, 
                graphArgs = Nothing, 
                isInteractive = False,
                isClientMode = False, 
                isServerMode = True, 
                serverArgs = Just (h,p) 
              } | (h == "") && (p /= 0) -> return cmdArgs
      CmdArgs { isHelpRequested = False, 
                graphArgs = Nothing, 
                isInteractive = True,
                isClientMode = False, 
                isServerMode = False, 
                serverArgs = Nothing 
              } -> return cmdArgs
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
                Option ['h'] ["help"] (NoArg (\opts -> opts { isHelpRequested = True })) "Print help",
                Option ['c'] ["client"] (NoArg (\opts -> opts { isClientMode = True })) "Client mode",
                Option ['s'] ["server"] (NoArg (\opts -> opts { isServerMode = True })) "Server mode",
                Option ['l'] ["location"] (OptArg (parseIO fillHost) "HOST") "Location/host of server",
                Option ['p'] ["port"] (OptArg (parseIO fillPort) "PORT") "Port of server"
             ]
   where parseIO f p opts = maybe opts (f opts) p
         fillInput opts@CmdArgs{graphArgs=ga} p = case ga of 
                                                     Just (i,o) -> opts { graphArgs = Just (p,o) }
                                                     Nothing -> opts { graphArgs = Just (p, "") }
         fillOutput opts@CmdArgs{graphArgs=ga} p = case ga of 
                                                      Just (i,o) -> opts { graphArgs = Just (i,p) }
                                                      Nothing -> opts { graphArgs = Just ("", p) }
         fillHost opts@CmdArgs{serverArgs=sa} pr = case sa of 
                                                      Just (h,p) -> opts { serverArgs = Just (pr,p) }
                                                      Nothing -> opts { serverArgs = Just (pr, 0) }
         fillPort opts@CmdArgs{serverArgs=sa} pr = let port = read pr in 
                                                      case sa of 
                                                         Just (h,p) -> opts { serverArgs = Just (h,port) }
                                                         Nothing -> opts { serverArgs = Just ("", port) }
      
handleCmdArgs :: CmdArgs -> IO()
handleCmdArgs args = do 
   case args of
      CmdArgs { isHelpRequested = True } -> putStrLn getUsageInfo
      CmdArgs { isServerMode = True, serverArgs = Just (_,p) } -> startEilerGaServer p
      CmdArgs { isClientMode = True, graphArgs = Just ga, serverArgs = Just sa } -> startEilerGaClient ga sa
      otherwise -> handleGaArgs args
   
----- server mode --------   
startEilerGaServer :: Int -> IO ()
startEilerGaServer port = do putStrLn $ "Starting eiler GA server on port " ++ (show port)
                             runEilerGaServer port getEilerResult  
                       
----- client mode --------  
                       
startEilerGaClient :: GraphArgs -> ServerArgs -> IO ()
startEilerGaClient (input, output) serverArgs = do putStrLn $ "Reading grpah from file " ++ input
                                                   inputGraph <- readFile input
                                                   result <- receiveEilerResult (T.pack inputGraph) serverArgs
                                                   putStrLn $ "Writing result to PNG image " ++ output
                                                   exportToPng (parseDotGraph result) output   
----------------------------------
                                            
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
