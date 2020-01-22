{-# LANGUAGE OverloadedStrings #-}
module EilerServer where

import Control.Monad.Except
import Data.Conduit
import Data.Conduit.Network
import Data.Graph
import Data.GraphViz
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import GraphIO
import Network.Socket



----- server --------   
runEilerGaServer :: Int -> (Graph -> Either String [Int]) -> IO ()
runEilerGaServer port eilerFunc = withSocketsDo $ runTCPServer (serverSettings port "*") (eilerGaServerApp eilerFunc)

eilerGaServerApp :: (Graph -> Either String [Int]) -> AppData -> IO()
eilerGaServerApp eilerFunc appData = do putStrLn "Start handling eiler GA request.."
                                        runConduit $
                                           appSource appData
                                           .| decodeDotGraph
                                           .| (findEilerPath eilerFunc)
                                           .| encodeDotGraph
                                           .| appSink appData 
                                        putStrLn "Eiler GA request handled"
                       
decodeDotGraph :: (MonadIO m) => ConduitT BS.ByteString (DotGraph Int) m ()
decodeDotGraph = do bsOpt <- await
                    case bsOpt of
                       Nothing -> return ()
                       Just bs -> yield $ bsToDotGraph bs
                       
encodeDotGraph :: (MonadIO m) => ConduitT (DotGraph Int) BS.ByteString m ()
encodeDotGraph = do gOpt <- await
                    case gOpt of
                       Nothing -> return ()
                       Just dg -> yield $ dotGraphToBS dg 
                       
findEilerPath :: (MonadIO m) => (Graph -> Either String [Int]) -> ConduitT (DotGraph Int) (DotGraph Int) m ()
findEilerPath eilerFunc = do gOpt <- await
                             case gOpt of
                                Nothing -> return ()
                                Just dg -> do let (srcGraph, adj) = parseFromDot dg
                                              liftIO $ putStrLn "Strat finding eiler path..."
                                              case eilerFunc srcGraph of
                                                 Left e -> liftIO $ putStrLn e
                                                 Right result -> do liftIO $ putStrLn "Eiler path was found."
                                                                    yield $ graphFromNodes adj result      
                       
----- client --------                                                                                                     
receiveEilerResult :: T.Text -> (String, Int) -> IO T.Text
receiveEilerResult graphText (host, port) = do putStrLn "Requesting result from server.."
                                               result <- withSocketsDo $ runTCPClient clSettings (eilerGaClientApp graphText)
                                               putStrLn "Result was received."
                                               return result
                                            where clSettings = clientSettings port (BS.pack host)
   

eilerGaClientApp :: T.Text -> AppData -> IO T.Text
eilerGaClientApp graphText appData = do putStrLn "Sending message to server..." 
                                        runConduit $ (yield $ textToBS graphText) .| appSink appData
                                        putStrLn "Receiving message from server..."
                                        runConduit $ appSource appData .| ( do Just result <- await
                                                                               return $ bsToText result)
                                                                               
------ server/client utils ------

bsToText :: BS.ByteString -> T.Text
bsToText = (TE.decodeUtf8).(LBS.fromStrict)

textToBS :: T.Text -> BS.ByteString
textToBS = (LBS.toStrict).(TE.encodeUtf8)

bsToDotGraph :: BS.ByteString -> DotGraph Int
bsToDotGraph = parseDotGraph.bsToText

dotGraphToBS :: DotGraph Int -> BS.ByteString
dotGraphToBS = textToBS.printDotGraph
