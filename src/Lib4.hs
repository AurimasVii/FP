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
import Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof, elements)

import Control.Monad.Trans.State.Strict (State, get, put)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import Data.List (isPrefixOf)
import Data.Char (isAlpha, isDigit)
import Control.Applicative
import Control.Monad.Free(Free (..))

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
      else throwE $ ("Expected keyword: " ++ kw)

parseSpaces :: Parser String
parseSpaces = do
  input <- lift get
  let (s, rest) = span (== ' ') input
  lift $ put rest
  return s

parseString :: Parser String
parseString = some parseLetter
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
parseDouble = ((\intPart _ fracPart -> read (intPart ++ "." ++ fracPart))
    <$> some parseDigit
    <*> parseKeyword "."
    <*> some parseDigit)
  <|> (read <$> some parseDigit)
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

parseState :: Parser Lib1.State
parseState = (Lib1.On <$ parseKeyword "on")
          <|> (Lib1.Off <$ parseKeyword "off")

parseAction :: Parser Lib1.Action
parseAction =
  ((\_ _ _ -> Lib1.TurnOnDevice)
    <$> parseKeyword "turn"
    <*> parseSpaces
    <*> parseKeyword "on") <|>
  ((\_ _ _ -> Lib1.TurnOffDevice)
    <$> parseKeyword "turn"
    <*> parseSpaces
    <*> parseKeyword "off") <|>
  ((\_ _ _ -> Lib1.SetBrightnessLevel)
    <$> parseKeyword "set"
    <*> parseSpaces
    <*> parseKeyword "brightness") <|>
  ((\_ _ _ -> Lib1.SetTemperatureLevel)
    <$> parseKeyword "set"
    <*> parseSpaces
    <*> parseKeyword "temperature")

-- | Parses user's input.
-- Yes, yes, yes. This is pretty much the same parser as in Lib3
-- It will be mostly a copy-paste because all <|>, <$>, <*> work
-- out of the box and only terminal (leaves) parsers will be changed.
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

parseDump :: Parser Lib1.Command
parseDump = (\_ _ _ -> Lib1.Dump Lib1.Examples)
  <$> parseKeyword "dump"
  <*> parseSpaces
  <*> parseKeyword "examples"

parseAdd :: Parser Lib1.Command
parseAdd = Lib1.Add <$> (addHouse <|> addRoom <|> addDevice)
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

parseRemove :: Parser Lib1.Command
parseRemove = Lib1.Remove <$> (removeHouse <|> removeRoom <|> removeDevice)
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

parseRename :: Parser Lib1.Command
parseRename = Lib1.Rename <$> (renameHouse <|> renameRoom <|> renameDevice)
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

parseSet :: Parser Lib1.Command
parseSet = Lib1.Set <$> (setBrightness <|> setTemperature <|> setState)
setBrightness :: Parser Lib1.SetCommand
setBrightness = (\_ _ a _ _ _ b -> Lib1.SetBrightness a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "brightness"
  <*> parseSpaces
  <*> parseDouble
setTemperature :: Parser Lib1.SetCommand
setTemperature = (\_ _ a _ _ _ b -> Lib1.SetTemperature a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "temperature"
  <*> parseSpaces
  <*> parseDouble
setState :: Parser Lib1.SetCommand
setState = (\_ _ a _ _ _ b -> Lib1.SetState a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseKeyword "state"
  <*> parseSpaces
  <*> parseState

parseControl :: Parser Lib1.Command
parseControl = Lib1.Control <$> (turnOn <|> turnOff)
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

parseReport :: Parser Lib1.Command
parseReport =  Lib1.Report <$> (reportHouse <|> reportRoom <|> reportDevice)
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
parseReportList = many (reportHouse <|> reportRoom <|> reportDevice)

parseSimulate :: Parser Lib1.Command
parseSimulate = (\_ _ _ -> Lib1.Simulate Lib1.SimulateDay)
    <$> parseKeyword "simulate"
    <*> parseSpaces
    <*> parseKeyword "day"

-- | This generates arbitrary (a.k.a random) commands for tests.
instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary = oneof [
    return (Lib1.Dump Lib1.Examples),

    Lib1.Add <$> oneof [
      Lib1.AddHouse <$> arbitrary,
      Lib1.AddRoom <$> arbitrary <*> arbitrary,
      Lib1.AddDevice <$> arbitrary <*> arbitrary
    ],

    Lib1.Remove <$> oneof [
      Lib1.RemoveHouse <$> arbitrary,
      Lib1.RemoveRoom <$> arbitrary <*> arbitrary,
      Lib1.RemoveDevice <$> arbitrary <*> arbitrary
    ],

    Lib1.Set <$> oneof [
      Lib1.SetBrightness <$> arbitrary <*> arbitrary,
      Lib1.SetTemperature <$> arbitrary <*> arbitrary,
      Lib1.SetState <$> arbitrary <*> elements [Lib1.On, Lib1.Off]
    ],

    Lib1.Rename <$> oneof [
      Lib1.RenameHouse <$> arbitrary <*> arbitrary,
      Lib1.RenameRoom <$> arbitrary <*> arbitrary,
      Lib1.RenameDevice <$> arbitrary <*> arbitrary
    ],

    Lib1.Control <$> oneof [
      Lib1.TurnOn <$> arbitrary,
      Lib1.TurnOff <$> arbitrary
    ],

    Lib1.Schedule <$> (Lib1.ScheduleAt <$> arbitrary <*> elements [Lib1.TurnOnDevice, Lib1.TurnOffDevice, Lib1.SetBrightnessLevel, Lib1.SetTemperatureLevel] <*> arbitrary),

    Lib1.Report <$> oneof [
      Lib1.ReportDevice <$> arbitrary,
      Lib1.ReportRoom <$> arbitrary <*> pure [],
      Lib1.ReportHouse <$> arbitrary <*> pure []
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
  fmap f (DumpCmd d next) = DumpCmd d (\a -> f (next a))
  fmap f (AddCmd cmd next) = AddCmd cmd (\a -> f (next a))
  fmap f (RemoveCmd cmd next) = RemoveCmd cmd (\a -> f (next a))
  fmap f (SetCmd cmd next) = SetCmd cmd (\a -> f (next a))
  fmap f (RenameCmd cmd next) = RenameCmd cmd (\a -> f (next a))
  fmap f (ControlCmd cmd next) = ControlCmd cmd (\a -> f (next a))
  fmap f (ScheduleCmd cmd next) = ScheduleCmd cmd (\a -> f (next a))
  fmap f (ReportCmd cmd next) = ReportCmd cmd (\a -> f (next a))
  fmap f (SimulateCmd cmd next) = SimulateCmd cmd (\a -> f (next a))

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