{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib4 (
    parseCommand,
    CommandDSL,
    CommandAlgebra(..),
    dump, add, remove, set, rename, control, schedule, report, simulate
) where

import qualified Lib1
import Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof, elements, listOf, choose)

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.List (isPrefixOf)
import Data.Char (isAlpha, isDigit)
import Control.Applicative (Alternative(..), some, many)
import Control.Monad.Free(Free (..))
import Control.Monad (MonadPlus, mzero, mplus)

type ErrorMsg = String
type Input = String
type Parser = ExceptT ErrorMsg (State Input)

parseKeyword :: String -> Parser String
parseKeyword kw = do
    input <- lift get
    let input' = dropWhile (== ' ') input
    if kw `isPrefixOf` input'
      then do
        lift $ put (drop (length kw) input')
        return kw
      else throwE ("Expected keyword: " ++ kw)

parseSpaces :: Parser String
parseSpaces = do
  input <- lift get
  let (s, rest) = span (== ' ') input
  lift $ put rest
  return s

parseString :: Parser String
parseString = ExceptT $ do
  stateBefore <- get
  result <- runExceptT (some parseLetter)
  case result of
    Right x -> return (Right x)
    Left err -> do
      put stateBefore
      return (Left err)
parseLetter :: Parser Char
parseLetter = do
    input <- lift get
    case input of
        [] -> throwE "A letter is expected but got empty input"
        (h:t) -> if isAlpha h
            then do
                lift (put t)
                return h
            else throwE $ "A letter is expected, but got " ++ [h]

parseDouble :: Parser Double
parseDouble = (\intPart _ fracPart -> read (intPart ++ "." ++ fracPart))
    <$> some parseDigit
    <*> parseKeyword "."
    <*> some parseDigit
  <|> read <$> some parseDigit
parseDigit :: Parser Char
parseDigit = do
  input <- lift get
  case input of
    [] -> throwE "Expected digit but got empty input"
    (h:t) -> if isDigit h
      then do
        lift (put t)
        return h
      else throwE $ "A digit is expected, but got " ++ [h]

-- BNF: <state> ::= "on" || "off"
parseState :: Parser Lib1.State
parseState = Lib1.On <$ parseKeyword "on"
          <|> Lib1.Off <$ parseKeyword "off"

-- BNF: <action> ::= "turn on" | "turn off" | "set brightness" | "set temperature"
parseAction :: Parser Lib1.Action
parseAction = ExceptT $ do
  stateBefore <- get
  let p1 = (\_ _ _ -> Lib1.TurnOnDevice)
        <$> parseKeyword "turn"
        <*> parseSpaces
        <*> parseKeyword "on"
      p2 = (\_ _ _ -> Lib1.TurnOffDevice)
        <$> parseKeyword "turn"
        <*> parseSpaces
        <*> parseKeyword "off"
      p3 = (\_ _ _ -> Lib1.SetBrightnessLevel)
        <$> parseKeyword "set"
        <*> parseSpaces
        <*> parseKeyword "brightness"
      p4 = (\_ _ _ -> Lib1.SetTemperatureLevel)
        <$> parseKeyword "set"
        <*> parseSpaces
        <*> parseKeyword "temperature"
  result1 <- runExceptT p1
  case result1 of
    Right x -> return (Right x)
    Left _ -> do
      put stateBefore
      result2 <- runExceptT p2
      case result2 of
        Right x -> return (Right x)
        Left _ -> do
          put stateBefore
          result3 <- runExceptT p3
          case result3 of
            Right x -> return (Right x)
            Left _ -> do
              put stateBefore
              runExceptT p4

-- BNF: <command> ::= 
  --    <add_command> 
  --  | <remove_command>
  --  | <set_command> 
  --  | <rename_command> 
  --  | <control_command> 
  --  | <schedule_command> 
  --  | <report_command> 
  --  | <simulate_command> 
  --  | "dump examples"

parseCommand :: Parser Lib1.Command
parseCommand =
   parseDump <|>
   parseAdd <|>
   parseRemove <|>
   parseRename <|>
   parseSet <|>
   parseControl <|>
   parseSchedule <|>
   parseReport <|>
   parseSimulate

-- BNF: "dump examples"
parseDump :: Parser Lib1.Command
parseDump = (\_ _ _ -> Lib1.Dump Lib1.Examples)
  <$> parseKeyword "dump"
  <*> parseSpaces
  <*> parseKeyword "examples"

-- BNF: <add_command> ::= 
      --   "add house " <house_name> 
      -- | "add room " <room_name> " to " <house_name> 
      -- | "add device " <device_name> " to " <room_or_device_name>
parseAdd :: Parser Lib1.Command
parseAdd = Lib1.Add <$> ExceptT (do
  stateBefore <- get
  result1 <- runExceptT addHouse
  case result1 of
    Right x -> return (Right x)
    Left _ -> do
      put stateBefore
      result2 <- runExceptT addRoom
      case result2 of
        Right x -> return (Right x)
        Left _ -> do
          put stateBefore
          runExceptT addDevice)
addHouse :: Parser Lib1.AddCommand
addHouse = (\_ _ _ _ a -> Lib1.AddHouse a)
  <$> parseKeyword "add"
  <*> parseSpaces
  <*> parseKeyword "house"
  <*> parseSpaces
  <*> parseString
addRoom :: Parser Lib1.AddCommand
addRoom = (\_ _ _ _ a _ _ _ b -> Lib1.AddRoom a b)
  <$> parseKeyword "add"
  <*> parseSpaces
  <*> parseKeyword "room"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> parseString
addDevice :: Parser Lib1.AddCommand
addDevice = (\_ _ _ _ a _ _ _ b -> Lib1.AddDevice a b)
  <$> parseKeyword "add"
  <*> parseSpaces
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> parseString

-- BNF: <remove_command> ::= 
      --   "remove house " <house_name> 
      -- | "remove room " <room_name> " from " <house_name>
      -- | "remove device " <device_name> " from " <room_name>
parseRemove :: Parser Lib1.Command
parseRemove = Lib1.Remove <$> ExceptT (do
  stateBefore <- get
  result1 <- runExceptT removeHouse
  case result1 of
    Right x -> return (Right x)
    Left _ -> do
      put stateBefore
      result2 <- runExceptT removeRoom
      case result2 of
        Right x -> return (Right x)
        Left _ -> do
          put stateBefore
          runExceptT removeDevice)
removeHouse :: Parser Lib1.RemoveCommand
removeHouse = (\_ _ _ _ a-> Lib1.RemoveHouse a)
  <$> parseKeyword "remove"
  <*> parseSpaces
  <*> parseKeyword "house"
  <*> parseSpaces
  <*> parseString
removeRoom :: Parser Lib1.RemoveCommand
removeRoom = (\_ _ _ _ a _ _ _ b -> Lib1.RemoveRoom a b)
  <$> parseKeyword "remove"
  <*> parseSpaces
  <*> parseKeyword "room"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "from"
  <*> parseSpaces
  <*> parseString
removeDevice :: Parser Lib1.RemoveCommand
removeDevice = (\_ _ _ _ a _ _ _ b -> Lib1.RemoveDevice a b)
  <$> parseKeyword "remove"
  <*> parseSpaces
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "from"
  <*> parseSpaces
  <*> parseString

-- BNF: <rename_command> ::= 
      --   "rename house " <old_name> " to " <new_name> 
      -- | "rename room " <old_name> " to " <new_name> 
      -- | "rename device " <old_name> " to " <new_name>
parseRename :: Parser Lib1.Command
parseRename = Lib1.Rename <$> ExceptT (do
  stateBefore <- get
  result1 <- runExceptT renameHouse
  case result1 of
    Right x -> return (Right x)
    Left _ -> do
      put stateBefore
      result2 <- runExceptT renameRoom
      case result2 of
        Right x -> return (Right x)
        Left _ -> do
          put stateBefore
          runExceptT renameDevice)
renameHouse :: Parser Lib1.RenameCommand
renameHouse = (\_ _ _ _ a _ _ _ b -> Lib1.RenameHouse a b)
  <$> parseKeyword "rename"
  <*> parseSpaces
  <*> parseKeyword "house"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> parseString
renameRoom :: Parser Lib1.RenameCommand
renameRoom = (\_ _ _ _ a _ _ _ b -> Lib1.RenameRoom a b)
  <$> parseKeyword "rename"
  <*> parseSpaces
  <*> parseKeyword "room"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> parseString
renameDevice :: Parser Lib1.RenameCommand
renameDevice = (\_ _ _ _ a _ _ _ b -> Lib1.RenameDevice a b)
  <$> parseKeyword "rename"
  <*> parseSpaces
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> parseString

--BNF: <set_command> ::= 
      --   "set device " <device_name> " brightness to " <value> 
      -- | "set device " <device_name> " temperature to " <value> 
      -- | "set device " <device_name> " state to " <state>
parseSet :: Parser Lib1.Command
parseSet = Lib1.Set <$> ExceptT (do
  stateBefore <- get
  result1 <- runExceptT setBrightness
  case result1 of
    Right x -> return (Right x)
    Left _ -> do
      put stateBefore
      result2 <- runExceptT setTemperature
      case result2 of
        Right x -> return (Right x)
        Left _ -> do
          put stateBefore
          runExceptT setState)
setBrightness :: Parser Lib1.SetCommand
setBrightness = (\_ _ _ _ a _ _ _ _ _ b -> Lib1.SetBrightness a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "brightness"
  <*> parseSpaces
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> parseDouble
setTemperature :: Parser Lib1.SetCommand
setTemperature = (\_ _ _ _ a _ _ _ _ _ b -> Lib1.SetTemperature a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "temperature"
  <*> parseSpaces
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> parseDouble
setState :: Parser Lib1.SetCommand
setState = (\_ _ _ _ a _ _ _ _ _ b -> Lib1.SetState a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "state"
  <*> parseSpaces
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> parseState

-- BNF: <control_command> ::= 
      --   "turn on " <device_name> 
      -- | "turn off " <device_name>
parseControl :: Parser Lib1.Command
parseControl = Lib1.Control <$> ExceptT (do
  stateBefore <- get
  result1 <- runExceptT turnOn
  case result1 of
    Right x -> return (Right x)
    Left _ -> do
      put stateBefore
      runExceptT turnOff)
turnOn :: Parser Lib1.ControlCommand
turnOn = (\_ _ _ _ a -> Lib1.TurnOn a)
  <$> parseKeyword "turn"
  <*> parseSpaces
  <*> parseKeyword "on"
  <*> parseSpaces
  <*> parseString
turnOff :: Parser Lib1.ControlCommand
turnOff = (\_ _ _ _ a -> Lib1.TurnOff a)
  <$> parseKeyword "turn"
  <*> parseSpaces
  <*> parseKeyword "off"
  <*> parseSpaces
  <*> parseString

-- BNF: <schedule_command> ::= "schedule " <device_name> <action> " " <value>
parseSchedule :: Parser Lib1.Command
parseSchedule = Lib1.Schedule <$> scheduleAt
scheduleAt :: Parser Lib1.ScheduleCommand
scheduleAt = (\_ _ a _ b _ c -> Lib1.ScheduleAt a b c)
  <$> parseKeyword "schedule"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseAction
  <*> parseSpaces
  <*> parseDouble

-- BNF: <report_command> ::= <report_house>
                  --  | <report_room>
                  --  | <report_device>
parseReport :: Parser Lib1.Command
parseReport =  Lib1.Report <$> ExceptT (do
  stateBefore <- get
  result1 <- runExceptT reportHouse
  case result1 of
    Right x -> return (Right x)
    Left _ -> do
      put stateBefore
      result2 <- runExceptT reportRoom
      case result2 of
        Right x -> return (Right x)
        Left _ -> do
          put stateBefore
          runExceptT reportDevice)
reportHouse :: Parser Lib1.ReportCommand
reportHouse =
  (\_ _ _ _ name _ reports -> Lib1.ReportHouse name reports)
    <$> parseKeyword "report"
    <*> parseSpaces
    <*> parseKeyword "house"
    <*> parseSpaces
    <*> parseString
    <*> parseSpaces
    <*> parseReportList
reportRoom :: Parser Lib1.ReportCommand
reportRoom =
  (\_ _ _ _ name _ reports -> Lib1.ReportRoom name reports)
    <$> parseKeyword "report"
    <*> parseSpaces
    <*> parseKeyword "room"
    <*> parseSpaces
    <*> parseString
    <*> parseSpaces
    <*> parseReportList
reportDevice :: Parser Lib1.ReportCommand
reportDevice =
  (\_ _ _ _ name -> Lib1.ReportDevice name)
    <$> parseKeyword "report"
    <*> parseSpaces
    <*> parseKeyword "device"
    <*> parseSpaces
    <*> parseString

-- Move parseReportList here, after report parsers are defined
parseReportList :: Parser [Lib1.ReportCommand]
parseReportList = many (ExceptT $ do
  stateBefore <- get
  result1 <- runExceptT reportHouse
  case result1 of
    Right x -> return (Right x)
    Left _ -> do
      put stateBefore
      result2 <- runExceptT reportRoom
      case result2 of
        Right x -> return (Right x)
        Left _ -> do
          put stateBefore
          runExceptT reportDevice)

-- BNF: <simulate_command> ::= "simulate day"
parseSimulate :: Parser Lib1.Command
parseSimulate = (\_ _ _ -> Lib1.Simulate Lib1.SimulateDay)
    <$> parseKeyword "simulate"
    <*> parseSpaces
    <*> parseKeyword "day"

-- Helper to generate positive doubles without scientific notation
positiveDouble :: Gen Double
positiveDouble = do
  -- Generate positive doubles without scientific notation
  -- Limit to reasonable range (0.1 to 1000) to avoid scientific notation
  intPart <- choose (1, 1000) :: Gen Int
  fracPart <- choose (0, 999) :: Gen Int
  let num = fromIntegral intPart + fromIntegral fracPart / 1000.0
  return num

-- Helper to generate non-empty strings (at least one letter)
nonEmptyString :: Gen String
nonEmptyString = do
  first <- elements ['a'..'z']
  rest <- listOf (elements ['a'..'z'])
  return (first : rest)

-- | This generates arbitrary (a.k.a random) commands for tests.
instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary = oneof [
    return (Lib1.Dump Lib1.Examples),

    Lib1.Add <$> oneof [
      Lib1.AddHouse <$> nonEmptyString,
      Lib1.AddRoom <$> nonEmptyString <*> nonEmptyString,
      Lib1.AddDevice <$> nonEmptyString <*> nonEmptyString
    ],

    Lib1.Remove <$> oneof [
      Lib1.RemoveHouse <$> nonEmptyString,
      Lib1.RemoveRoom <$> nonEmptyString <*> nonEmptyString,
      Lib1.RemoveDevice <$> nonEmptyString <*> nonEmptyString
    ],

    Lib1.Set <$> oneof [
      Lib1.SetBrightness <$> nonEmptyString <*> positiveDouble,
      Lib1.SetTemperature <$> nonEmptyString <*> positiveDouble,
      Lib1.SetState <$> nonEmptyString <*> elements [Lib1.On, Lib1.Off]
    ],

    Lib1.Rename <$> oneof [
      Lib1.RenameHouse <$> nonEmptyString <*> nonEmptyString,
      Lib1.RenameRoom <$> nonEmptyString <*> nonEmptyString,
      Lib1.RenameDevice <$> nonEmptyString <*> nonEmptyString
    ],

    Lib1.Control <$> oneof [
      Lib1.TurnOn <$> nonEmptyString,
      Lib1.TurnOff <$> nonEmptyString
    ],

    Lib1.Schedule <$> (Lib1.ScheduleAt <$> nonEmptyString <*> elements [Lib1.TurnOnDevice, Lib1.TurnOffDevice, Lib1.SetBrightnessLevel, Lib1.SetTemperatureLevel] <*> positiveDouble),

    Lib1.Report <$> oneof [
      Lib1.ReportDevice <$> nonEmptyString,
      Lib1.ReportRoom <$> nonEmptyString <*> pure [],
      Lib1.ReportHouse <$> nonEmptyString <*> pure []
    ],

    pure (Lib1.Simulate Lib1.SimulateDay)]

-- Free Monad DSL
data CommandAlgebra next = DumpCmd Lib1.Dumpable (() -> next)
                         | AddCmd Lib1.AddCommand (() -> next)
                         | RemoveCmd Lib1.RemoveCommand (() -> next)
                         | SetCmd Lib1.SetCommand (() -> next)
                         | RenameCmd Lib1.RenameCommand (() -> next)
                         | ControlCmd Lib1.ControlCommand (() -> next)
                         | ScheduleCmd Lib1.ScheduleCommand (() -> next)
                         | ReportCmd Lib1.ReportCommand (String -> next)
                         | SimulateCmd Lib1.SimulateCommand (() -> next)

instance Functor CommandAlgebra where
  fmap :: (a -> b) -> CommandAlgebra a -> CommandAlgebra b
  fmap f (DumpCmd d next) = DumpCmd d (f . next)
  fmap f (AddCmd cmd next) = AddCmd cmd (f . next)
  fmap f (RemoveCmd cmd next) = RemoveCmd cmd (f . next)
  fmap f (SetCmd cmd next) = SetCmd cmd (f . next)
  fmap f (RenameCmd cmd next) = RenameCmd cmd (f . next)
  fmap f (ControlCmd cmd next) = ControlCmd cmd (f . next)
  fmap f (ScheduleCmd cmd next) = ScheduleCmd cmd (f . next)
  fmap f (ReportCmd cmd next) = ReportCmd cmd (f . next)
  fmap f (SimulateCmd cmd next) = SimulateCmd cmd (f . next)

type CommandDSL a = Free CommandAlgebra a

-- DSL methods
dump :: Lib1.Dumpable -> CommandDSL ()
dump d = Free (DumpCmd d Pure)

add :: Lib1.AddCommand -> CommandDSL ()
add cmd = Free (AddCmd cmd Pure)

remove :: Lib1.RemoveCommand -> CommandDSL ()
remove cmd = Free (RemoveCmd cmd Pure)

set :: Lib1.SetCommand -> CommandDSL ()
set cmd = Free (SetCmd cmd Pure)

rename :: Lib1.RenameCommand -> CommandDSL ()
rename cmd = Free (RenameCmd cmd Pure)

control :: Lib1.ControlCommand -> CommandDSL ()
control cmd = Free (ControlCmd cmd Pure)

schedule :: Lib1.ScheduleCommand -> CommandDSL ()
schedule cmd = Free (ScheduleCmd cmd Pure)

report :: Lib1.ReportCommand -> CommandDSL String
report cmd = Free (ReportCmd cmd Pure)

simulate :: Lib1.SimulateCommand -> CommandDSL ()
simulate cmd = Free (SimulateCmd cmd Pure)