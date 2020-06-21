{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE FlexibleContexts #-}
module Main where

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
import EilerServer
import qualified Data.Text.Lazy as T
import Data.GraphViz
import Options.Applicative

type GraphArgs = (String, String)
type ServerArgs = (String, Int)

data CmdArgs = Interactive
              | LocalArgs GraphArgs
              | ServerMode Int
              | ClientMode ServerArgs GraphArgs 
              deriving Show               
             
main::IO ()
main = do 
   args <- execParser cmdArgsInfo
   handleCmdArgs args

------ cmd args parsing -------------
cmdArgsInfo :: ParserInfo CmdArgs
cmdArgsInfo = info (cmdArgsParser <**> helper) (fullDesc <> header "Eiler path evaluator")

cmdArgsParser :: Parser CmdArgs
cmdArgsParser = localArgsParser <|> serverModeParser <|> clientModeParser <|> interactiveArgsParser

interactiveArgsParser :: Parser CmdArgs
interactiveArgsParser = pure Interactive

localArgsParser :: Parser CmdArgs
localArgsParser = LocalArgs <$> graphArgsParser

graphArgsParser :: Parser GraphArgs
graphArgsParser = (,) 
               <$> strOption (
                      long "input"
                      <> short 'i'
                      <> metavar "INPUT"
                      <> help "Input graph .dot filename"    
                   )
              <*> strOption (
                      long "output"
                      <> short 'o'
                      <> metavar "OUTPUT"
                      <> help "Output PNG filename"    
                  )

serverModeParser :: Parser CmdArgs
serverModeParser = flag' () (
                      long "server"
                      <> short 's'
                      <> help "Server mode"    
                   ) *> (ServerMode <$> portOption)

clientModeParser :: Parser CmdArgs
clientModeParser = flag' () (
                      long "client"
                      <> short 'c'
                      <> help "Client mode"    
                   ) *> (ClientMode <$> serverArgsParser <*> graphArgsParser)

serverArgsParser :: Parser ServerArgs
serverArgsParser = (,) 
               <$> strOption (
                      long "location"
                      <> short 'l'
                      <> metavar "LOCATION"
                      <> help "Location/host of server"    
                   )
              <*> portOption
 
portOption :: Parser Int 
portOption = option auto (
                      long "port"
                      <> short 'p'
                      <> metavar "PORT"
                      <> help "Port of server"    
                  )      

----------- handling -------------      
handleCmdArgs :: CmdArgs -> IO()
handleCmdArgs Interactive = readGraphParamsInteractive >>= handleLocalGraphArgs
handleCmdArgs (LocalArgs grArgs) = handleLocalGraphArgs grArgs
handleCmdArgs (ServerMode port) = startEilerGaServer port
handleCmdArgs (ClientMode srvArgs grArgs) = startEilerGaClient grArgs srvArgs

   
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
handleLocalGraphArgs :: GraphArgs -> IO ()
handleLocalGraphArgs (srcPath, destPath) = do
   (g, adj) <- readGraph srcPath
   case getEilerResult g of
      Left e -> putStrLn e
      Right result -> do writeVertexes destPath adj result
                         putStrLn $ "Result was written into " ++ destPath                                     

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
