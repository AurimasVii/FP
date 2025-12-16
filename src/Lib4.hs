{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib4 (
    parseCommand,
    CommandDSL,
    CommandAlgebra(..),
    dslDump, dslAdd, dslRemove, dslSet, dslRename, dslControl, dslSchedule, dslReport, dslSimulate
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

instance {-# OVERLAPPING #-} Alternative Parser where
  empty = throwE "empty" 
  p <|> q = ExceptT $ do
    stateBefore <- get
    result <- runExceptT p
    case result of
      Left _ -> do
        put stateBefore
        runExceptT q
      Right val -> return (Right val)

--galima naudot 'helper' funkcija vietoj overlaping alternative:
--infixl 3 <||>
--(<||>) :: Parser a -> Parser a -> Parser a
--p1 <||> p2 = ExceptT $ do
--    s <- get
--    result1 <- Control.Monad.Trans.Except.runExceptT p1
--    case result1 of
--        Right x -> return $ Right x
--        Left _ -> do
--            put s  -- resetina state
--            Control.Monad.Trans.Except.runExceptT p2

-- ir tada pvz parseAction (nes reikia nepamirsti state) butu:
--parseAction :: Parser Lib1.Action
--parseAction =
--  ((\_ _ _ -> Lib1.TurnOnDevice)
--    <$> parseKeyword "turn"
--    <*> parseSpaces
--    <*> parseKeyword "on") <||>
--  ((\_ _ _ -> Lib1.TurnOffDevice)
--    <$> parseKeyword "turn"
--    <*> parseSpaces
--    <*> parseKeyword "off") <||>
--  ((\_ _ _ -> Lib1.SetBrightnessLevel)
--    <$> parseKeyword "set"
--    <*> parseSpaces
--    <*> parseKeyword "brightness") <||>
--  ((\_ _ _ -> Lib1.SetTemperatureLevel)
--    <$> parseKeyword "set"
--    <*> parseSpaces
--    <*> parseKeyword "temperature")

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

parseCommand :: Parser Lib1.Command
parseCommand =
   dump <|>
   add <|>
   remove <|>
   rename <|>
   set <|>
   control <|>
   schedule <|>
   report <|>
   simulate

dump :: Parser Lib1.Command
dump = (\_ _ _ -> Lib1.Dump Lib1.Examples)
  <$> parseKeyword "dump"
  <*> parseSpaces
  <*> parseKeyword "examples"

add :: Parser Lib1.Command
add = Lib1.Add <$> (addHouse <|> addRoom <|> addDevice)

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

remove :: Parser Lib1.Command
remove = Lib1.Remove <$> (removeHouse <|> removeRoom <|> removeDevice)

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

rename :: Parser Lib1.Command
rename = Lib1.Rename <$> (renameHouse <|> renameRoom <|> renameDevice)

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

set :: Parser Lib1.Command
set = Lib1.Set <$> (setBrightness <|> setTemperature <|> setState)

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


control :: Parser Lib1.Command
control = Lib1.Control <$> (turnOn <|> turnOff)

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

schedule :: Parser Lib1.Command
schedule = Lib1.Schedule <$> scheduleAt

scheduleAt :: Parser Lib1.ScheduleCommand
scheduleAt = (\_ _ a _ b _ c -> Lib1.ScheduleAt a b c)
  <$> parseKeyword "schedule"
  <*> parseSpaces
  <*> parseString
  <*> parseSpaces
  <*> parseAction
  <*> parseSpaces
  <*> parseDouble

report :: Parser Lib1.Command
report = Lib1.Report <$> (reportHouse <|> reportRoom <|> reportDevice)

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

parseReportList :: Parser [Lib1.ReportCommand]
parseReportList = many (reportHouse <|> reportRoom <|> reportDevice)

simulate :: Parser Lib1.Command
simulate = (\_ _ _ -> Lib1.Simulate Lib1.SimulateDay)
    <$> parseKeyword "simulate"
    <*> parseSpaces
    <*> parseKeyword "day"


positiveDouble :: Gen Double
positiveDouble = do
  intPart <- choose (1, 1000) :: Gen Int
  fracPart <- choose (0, 999) :: Gen Int
  let num = fromIntegral intPart + fromIntegral fracPart / 1000.0
  return num

nonEmptyString :: Gen String
nonEmptyString = do
  first <- elements ['a'..'z']
  rest <- listOf (elements ['a'..'z'])
  return (first : rest)

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

dslDump :: Lib1.Dumpable -> CommandDSL ()
dslDump d = Free (DumpCmd d Pure)

dslAdd :: Lib1.AddCommand -> CommandDSL ()
dslAdd cmd = Free (AddCmd cmd Pure)

dslRemove :: Lib1.RemoveCommand -> CommandDSL ()
dslRemove cmd = Free (RemoveCmd cmd Pure)

dslSet :: Lib1.SetCommand -> CommandDSL ()
dslSet cmd = Free (SetCmd cmd Pure)

dslRename :: Lib1.RenameCommand -> CommandDSL ()
dslRename cmd = Free (RenameCmd cmd Pure)

dslControl :: Lib1.ControlCommand -> CommandDSL ()
dslControl cmd = Free (ControlCmd cmd Pure)

dslSchedule :: Lib1.ScheduleCommand -> CommandDSL ()
dslSchedule cmd = Free (ScheduleCmd cmd Pure)

dslReport :: Lib1.ReportCommand -> CommandDSL String
dslReport cmd = Free (ReportCmd cmd Pure)

dslSimulate :: Lib1.SimulateCommand -> CommandDSL ()
dslSimulate cmd = Free (SimulateCmd cmd Pure)