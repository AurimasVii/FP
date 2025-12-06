{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Free (Free(..), foldFree)
import Control.Monad.Trans.State.Strict (StateT, runStateT, get, put, modify)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Lib1
import qualified Lib2
import qualified Lib3
import qualified Lib4
import Network.HTTP.Client
import Network.HTTP.Types (HeaderName, hContentType)
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

httpInterpreter :: Lib4.CommandDSL a -> IO a
httpInterpreter = foldFree httpAlgebra
  where
    httpAlgebra :: Lib4.CommandAlgebra x -> IO x
    httpAlgebra (Lib4.DumpCmd _ next) = return (next ())
    httpAlgebra (Lib4.AddCmd cmd next) = do
      _ <- sendCommandToServer (Lib1.Add cmd)
      return (next ())
    httpAlgebra (Lib4.RemoveCmd cmd next) = do
      _ <- sendCommandToServer (Lib1.Remove cmd)
      return (next ())
    httpAlgebra (Lib4.SetCmd cmd next) = do
      _ <- sendCommandToServer (Lib1.Set cmd)
      return (next ())
    httpAlgebra (Lib4.RenameCmd cmd next) = do
      _ <- sendCommandToServer (Lib1.Rename cmd)
      return (next ())
    httpAlgebra (Lib4.ControlCmd cmd next) = do
      _ <- sendCommandToServer (Lib1.Control cmd)
      return (next ())
    httpAlgebra (Lib4.ScheduleCmd cmd next) = do
      _ <- sendCommandToServer (Lib1.Schedule cmd)
      return (next ())
    httpAlgebra (Lib4.ReportCmd cmd next) = do
      result <- sendCommandToServer (Lib1.Report cmd)
      return (next result)
    httpAlgebra (Lib4.SimulateCmd cmd next) = do
      _ <- sendCommandToServer (Lib1.Simulate cmd)
      return (next ())

    sendCommandToServer :: Lib1.Command -> IO String
    sendCommandToServer cmd = do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:8080"
      let cmdStr = Lib2.toCliCommand cmd
          req = request
            { method = B8.pack "POST"
            , requestBody = RequestBodyLBS (BL.fromStrict $ B8.pack cmdStr)
            , requestHeaders = [(HTTP.hContentType, B8.pack "text/plain")]
            }
      response <- httpLbs req manager
      return $ B8.unpack (BL.toStrict $ responseBody response)

-- State Monad Interpreter
-- This interpreter implements domain functionality directly using State monad
type AppState = Lib3.State

stateInterpreter :: Lib4.CommandDSL a -> StateT AppState IO a
stateInterpreter = foldFree stateAlgebra
  where
    stateAlgebra :: Lib4.CommandAlgebra x -> StateT AppState IO x
    stateAlgebra (Lib4.DumpCmd Lib1.Examples next) = do
      liftIO $ mapM_ putStrLn ("Examples:" : map Lib2.toCliCommand Lib1.examples)
      return (next ())
    stateAlgebra (Lib4.AddCmd cmd next) = do
      stateExecAdd cmd
      return (next ())
    stateAlgebra (Lib4.RemoveCmd cmd next) = do
      stateExecRemove cmd
      return (next ())
    stateAlgebra (Lib4.SetCmd cmd next) = do
      stateExecSet cmd
      return (next ())
    stateAlgebra (Lib4.RenameCmd cmd next) = do
      stateExecRename cmd
      return (next ())
    stateAlgebra (Lib4.ControlCmd cmd next) = do
      stateExecControl cmd
      return (next ())
    stateAlgebra (Lib4.ScheduleCmd cmd next) = do
      stateExecSchedule cmd
      return (next ())
    stateAlgebra (Lib4.ReportCmd cmd next) = do
      result <- stateExecReport cmd
      return (next result)
    stateAlgebra (Lib4.SimulateCmd cmd next) = do
      stateExecSimulate cmd
      return (next ())

    stateExecAdd :: Lib1.AddCommand -> StateT AppState IO ()
    stateExecAdd (Lib1.AddHouse hName) = modify $ \st ->
      let newHouse = Lib3.House hName []
      in st { Lib3.houses = newHouse : Lib3.houses st }
    stateExecAdd (Lib1.AddRoom rName hName) = modify $ \st ->
      let updateHouse (Lib3.House name rooms)
            | name == hName = Lib3.House name (Lib3.Room rName [] : rooms)
            | otherwise = Lib3.House name rooms
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }
    stateExecAdd (Lib1.AddDevice dName rName) = modify $ \st ->
      let updateRoom (Lib3.Room name devices)
            | name == rName = Lib3.Room name (Lib3.Device dName Lib1.Off Nothing Nothing : devices)
            | otherwise = Lib3.Room name devices
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }

    stateExecRemove :: Lib1.RemoveCommand -> StateT AppState IO ()
    stateExecRemove (Lib1.RemoveHouse hName) = modify $ \st ->
      st { Lib3.houses = filter (\(Lib3.House name _) -> name /= hName) (Lib3.houses st) }
    stateExecRemove (Lib1.RemoveRoom rName hName) = modify $ \st ->
      let updateHouse (Lib3.House name rooms)
            | name == hName = Lib3.House name (filter (\(Lib3.Room rName' _) -> rName' /= rName) rooms)
            | otherwise = Lib3.House name rooms
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }
    stateExecRemove (Lib1.RemoveDevice dName rName) = modify $ \st ->
      let updateRoom (Lib3.Room name devices)
            | name == rName = Lib3.Room name (filter (\(Lib3.Device dName' _ _ _) -> dName' /= dName) devices)
            | otherwise = Lib3.Room name devices
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }

    stateExecSet :: Lib1.SetCommand -> StateT AppState IO ()
    stateExecSet (Lib1.SetBrightness dName val) = modify $ \st ->
      let updateDevice (Lib3.Device name status brightness temp)
            | name == dName = Lib3.Device name status (Just val) temp
            | otherwise = Lib3.Device name status brightness temp
          updateRoom (Lib3.Room name devices) = Lib3.Room name (map updateDevice devices)
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }
    stateExecSet (Lib1.SetTemperature dName val) = modify $ \st ->
      let updateDevice (Lib3.Device name status brightness temp)
            | name == dName = Lib3.Device name status brightness (Just val)
            | otherwise = Lib3.Device name status brightness temp
          updateRoom (Lib3.Room name devices) = Lib3.Room name (map updateDevice devices)
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }
    stateExecSet (Lib1.SetState dName state) = modify $ \st ->
      let updateDevice (Lib3.Device name status brightness temp)
            | name == dName = Lib3.Device name state brightness temp
            | otherwise = Lib3.Device name status brightness temp
          updateRoom (Lib3.Room name devices) = Lib3.Room name (map updateDevice devices)
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }

    stateExecRename :: Lib1.RenameCommand -> StateT AppState IO ()
    stateExecRename (Lib1.RenameHouse oldName newName) = modify $ \st ->
      let updateHouse (Lib3.House name rooms)
            | name == oldName = Lib3.House newName rooms
            | otherwise = Lib3.House name rooms
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }
    stateExecRename (Lib1.RenameRoom oldName newName) = modify $ \st ->
      let updateRoom (Lib3.Room name devices)
            | name == oldName = Lib3.Room newName devices
            | otherwise = Lib3.Room name devices
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }
    stateExecRename (Lib1.RenameDevice oldName newName) = modify $ \st ->
      let updateDevice (Lib3.Device name status brightness temp)
            | name == oldName = Lib3.Device newName status brightness temp
            | otherwise = Lib3.Device name status brightness temp
          updateRoom (Lib3.Room name devices) = Lib3.Room name (map updateDevice devices)
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }

    stateExecControl :: Lib1.ControlCommand -> StateT AppState IO ()
    stateExecControl (Lib1.TurnOn dName) = modify $ \st ->
      let updateDevice (Lib3.Device name status brightness temp)
            | name == dName = Lib3.Device name Lib1.On brightness temp
            | otherwise = Lib3.Device name status brightness temp
          updateRoom (Lib3.Room name devices) = Lib3.Room name (map updateDevice devices)
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }
    stateExecControl (Lib1.TurnOff dName) = modify $ \st ->
      let updateDevice (Lib3.Device name status brightness temp)
            | name == dName = Lib3.Device name Lib1.Off brightness temp
            | otherwise = Lib3.Device name status brightness temp
          updateRoom (Lib3.Room name devices) = Lib3.Room name (map updateDevice devices)
          updateHouse (Lib3.House name rooms) = Lib3.House name (map updateRoom rooms)
      in st { Lib3.houses = map updateHouse (Lib3.houses st) }

    stateExecSchedule :: Lib1.ScheduleCommand -> StateT AppState IO ()
    stateExecSchedule (Lib1.ScheduleAt dName actionNew timeNew) = modify $ \st ->
      let newItem = Lib3.ScheduleItem {
            Lib3.targetedDevice = dName,
            Lib3.action = actionNew,
            Lib3.time = timeNew
          }
      in st { Lib3.schedules = newItem : Lib3.schedules st }

    stateExecReport :: Lib1.ReportCommand -> StateT AppState IO String
    stateExecReport (Lib1.ReportHouse hName _) = do
      st <- get
      let Lib3.State {Lib3.houses = hs} = st
      case filter (\(Lib3.House name _) -> name == hName) hs of
        [] -> return $ "No such house: " ++ hName
        (Lib3.House hName' rooms' : _) -> do
          let lines = ("House: " ++ hName') :
                      concatMap (\(Lib3.Room rName devices') -> 
                        ("  Room: " ++ rName) :
                        map (\(Lib3.Device dName status brightness temp) ->
                          "    Device: " ++ dName ++
                          " [" ++ show status ++
                          maybe "" (\b -> ", Brightness=" ++ show b) brightness ++
                          maybe "" (\t -> ", Temperature=" ++ show t) temp ++ "]"
                        ) devices'
                      ) rooms'
          return $ unlines lines
    stateExecReport (Lib1.ReportRoom rName _) = do
      st <- get
      let Lib3.State {Lib3.houses = hs} = st
          allRooms = concatMap (\(Lib3.House _ rs) -> rs) hs
      case filter (\(Lib3.Room name _) -> name == rName) allRooms of
        [] -> return $ "No such room: " ++ rName
        (Lib3.Room rName' devices' : _) -> do
          let lines = ("Room: " ++ rName') :
                      map (\(Lib3.Device dName status brightness temp) ->
                        "  Device: " ++ dName ++
                        " [" ++ show status ++
                        maybe "" (\b -> ", Brightness=" ++ show b) brightness ++
                        maybe "" (\t -> ", Temperature=" ++ show t) temp ++ "]"
                      ) devices'
          return $ unlines lines
    stateExecReport (Lib1.ReportDevice dName) = do
      st <- get
      let Lib3.State {Lib3.houses = hs} = st
          allDevices = concatMap (\(Lib3.Room _ devices') -> devices') 
                      (concatMap (\(Lib3.House _ rs) -> rs) hs)
      case filter (\(Lib3.Device name _ _ _) -> name == dName) allDevices of
        [] -> return $ "No such device: " ++ dName
        (Lib3.Device dName' status brightness temp : _) -> return $
          "Device: " ++ dName' ++
          " [" ++ show status ++
          maybe "" (\b -> ", Brightness=" ++ show b) brightness ++
          maybe "" (\t -> ", Temperature=" ++ show t) temp ++ "]"

    stateExecSimulate :: Lib1.SimulateCommand -> StateT AppState IO ()
    stateExecSimulate Lib1.SimulateDay = do
      st <- get
      let Lib3.State {Lib3.schedules = scheds} = st
      if null scheds
        then liftIO $ putStrLn "No scheduled actions to simulate."
        else do
          liftIO $ putStrLn "Simulating day"
          let updatedState = Lib3.simulateDay st
          put updatedState
          liftIO $ putStrLn "Simulation complete. All scheduled actions have been applied."

exampleProgram :: Lib4.CommandDSL String
exampleProgram = do
  Lib4.dump Lib1.Examples
  Lib4.add (Lib1.AddHouse "TestHouse")
  Lib4.add (Lib1.AddRoom "TestRoom" "TestHouse")
  Lib4.add (Lib1.AddDevice "TestDevice" "TestRoom")
  Lib4.report (Lib1.ReportHouse "TestHouse" [])

main :: IO ()
main = do
  putStrLn "=== Running with HTTP interpreter ==="
  result1 <- httpInterpreter exampleProgram
  putStrLn result1
  putStrLn ""
  
  putStrLn "=== Running with State interpreter ==="
  (result2, finalState) <- runStateT (stateInterpreter exampleProgram) Lib3.emptyState
  putStrLn result2
  putStrLn $ "Final state: " ++ show finalState

