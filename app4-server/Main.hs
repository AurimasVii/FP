{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Trans.State.Strict (runState)
import Control.Monad.Trans.Except (runExceptT)
import qualified Lib1
import qualified Lib2
import qualified Lib3
import qualified Lib4
import Web.Scotty
import Network.HTTP.Types (status400)
import Control.Monad.IO.Class (liftIO)
import System.Exit
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Concurrent (newChan, forkIO, threadDelay)
import Control.Monad (forever)
import Control.Exception (finally)
import Data.Text.Lazy (pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

executeWithOutput :: TVar Lib3.State -> Lib1.Command -> IO String
executeWithOutput stateVar command = case command of
  Lib1.Dump Lib1.Examples -> do
    return $ unlines ("Examples:" : map Lib2.toCliCommand Lib1.examples)
  Lib1.Report (Lib1.ReportHouse hName _) -> do
    st <- readTVarIO stateVar
    let Lib3.State {Lib3.houses = hs} = st
    case filter (\(Lib3.House name _) -> name == hName) hs of
      [] -> return $ "No such house: " ++ hName
      (Lib3.House hName' rooms' : _) -> do
        let lines' = ("House: " ++ hName') :
                    concatMap (\(Lib3.Room rName devices') -> 
                      ("  Room: " ++ rName) :
                      map (\(Lib3.Device dName deviceStatus brightness temp) ->
                        "    Device: " ++ dName ++
                        " [" ++ show deviceStatus ++
                        maybe "" (\b -> ", Brightness=" ++ show b) brightness ++
                        maybe "" (\t -> ", Temperature=" ++ show t) temp ++ "]"
                      ) devices'
                    ) rooms'
        return $ unlines lines'
  Lib1.Report (Lib1.ReportRoom rName _) -> do
    st <- readTVarIO stateVar
    let Lib3.State {Lib3.houses = hs} = st
        allRooms = concatMap (\(Lib3.House _ rs) -> rs) hs
    case filter (\(Lib3.Room name _) -> name == rName) allRooms of
      [] -> return $ "No such room: " ++ rName
      (Lib3.Room rName' devices' : _) -> do
        let lines' = ("Room: " ++ rName') :
                    map (\(Lib3.Device dName status brightness temp) ->
                      "  Device: " ++ dName ++
                      " [" ++ show status ++
                      maybe "" (\b -> ", Brightness=" ++ show b) brightness ++
                      maybe "" (\t -> ", Temperature=" ++ show t) temp ++ "]"
                    ) devices'
        return $ unlines lines'
  Lib1.Report (Lib1.ReportDevice dName) -> do
    st <- readTVarIO stateVar
    let Lib3.State {Lib3.houses = hs} = st
        allDevices = concatMap (\(Lib3.Room _ devices') -> devices') 
                    (concatMap (\(Lib3.House _ rs) -> rs) hs)
    case filter (\(Lib3.Device name _ _ _) -> name == dName) allDevices of
      [] -> return $ "No such device: " ++ dName
      (Lib3.Device dName' deviceStatus brightness temp : _) -> return $
        "Device: " ++ dName' ++
        " [" ++ show deviceStatus ++
        maybe "" (\b -> ", Brightness=" ++ show b) brightness ++
        maybe "" (\t -> ", Temperature=" ++ show t) temp ++ "]"
  Lib1.Simulate Lib1.SimulateDay -> do
    st <- readTVarIO stateVar
    let Lib3.State {Lib3.schedules = scheds} = st
    if null scheds
      then return "No scheduled actions to simulate."
      else do
        Lib3.execute stateVar command
        return "Simulating day\nSimulation complete. All scheduled actions have been applied."
  _ -> do
    Lib3.execute stateVar command
    return ""

failOnError :: IO (Either String a) -> IO a
failOnError action = do
  result <- action
  case result of
    Right a -> return a
    Left m -> putStrLn ("Fatal error: " ++ m) >> exitFailure

main :: IO ()
main = do
  state <- newTVarIO Lib3.emptyState
  chan <- newChan
  _ <- forkIO $ Lib3.storageOpLoop chan
  _ <- failOnError $ Lib3.load chan state
  _ <- forkIO $ forever $ threadDelay 1000000 >> failOnError (Lib3.save chan state)
  putStrLn "Server starting on port 8080..."
  putStrLn "State persistence: Loaded from state.txt, saving every 1 second..."
  -- Save on shutdown
  finally
    (scotty 8080 $ do
      get "/" $ do
        text $ pack "Server is running. Send POST requests with commands in ToCliCommand format."
      
      post "/" $ do
        cmdStrBS <- body
        let cmdStr' = B8.unpack (BL.toStrict cmdStrBS)
        case runState (runExceptT Lib4.parseCommand) cmdStr' of
          (Right cmd, remaining) | null (dropWhile (== ' ') remaining) -> do
            -- Command parsed successfully
            output <- liftIO $ executeWithOutput state cmd
            text $ pack output
          (Right _, remaining) -> do
            let err = "Unconsumed input after command: '" ++ take 30 (dropWhile (== ' ') remaining) ++ "'"
            status status400
            text $ pack err
          (Left err, _) -> do
            status status400
            text $ pack ("Parse error: " ++ err))
    (do
      putStrLn "\nShutting down... Saving state..."
      _ <- failOnError (Lib3.save chan state)
      putStrLn "Goodbye!")