{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1

import Control.Concurrent.STM.TVar(TVar)
import Control.Concurrent (Chan, readChan)
import Control.Applicative
import Data.Char (isAlpha, isDigit)
import Data.List (isPrefixOf)

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

-- | Parses user's input.
-- Yes, this is pretty much the same parser as in Lib2
-- but with a bit different requirements:
-- 1) It must implement Functor, Applicative and Alternative
-- 2) It must NOT implement Monad, no do-notations
-- 3) pmap with andN become <$> <*>
-- 4) orElse becomes <|>
-- 5) many and many1 become many and some
-- Yes, it will be mostly a copy-paste but an easy one
-- if Lib2 was implemented correctly.

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f functor = Parser $ \input ->
    case runParser functor input of
        Left e -> Left e
        Right (v, r) -> Right (f v, r)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  af <*> aa = Parser $ \input ->
    case runParser af input of
        Left e1 -> Left e1
        Right (f, r1) ->
            case runParser aa r1 of
                Left e2 -> Left e2
                Right (a, r2) -> Right (f a, r2)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left "No alternatives"
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
        Right r2 -> Right r2
        Left e1 ->
            case runParser p2 input of
                Right r2 -> Right r2
                Left e2 -> Left $ e1 ++ "; " ++ e2 

parseKeyword :: String -> Parser String
parseKeyword kw = Parser $ \input ->
  let input' = dropWhile (== ' ') input
  in if kw `isPrefixOf` input'
       then Right (kw, drop (length kw) input')
       else Left ("Expected keyword: " ++ kw)


parseSpaces :: Parser String
parseSpaces = Parser $ \input ->
  let (s, rest) = span (== ' ') input
  in Right (s, rest)

skipWhitespace :: Parser String
skipWhitespace = Parser $ \input ->
  let (spaces, rest) = span (`elem` [' ', '\t', '\n', '\r']) input
  in Right (spaces, rest)

parseString :: Parser String
parseString = some (Parser $ \case
  (x:xs) | isAlpha x -> Right (x, xs)
  [] -> Left "Expected letter"
  (x:_) -> Left ("Unexpected char: " ++ [x]))

parseDigit :: Parser Char
parseDigit = Parser $ \case
  (x:xs) | isDigit x -> Right (x, xs)
  [] -> Left "Expected digit"
  (x:_) -> Left ("Unexpected char: " ++ [x])

parseDouble :: Parser Double
parseDouble = ((\intPart _ fracPart -> read(intPart ++ "." ++ fracPart))
    <$> some parseDigit
    <*> parseKeyword "."
    <*> some parseDigit)
  <|> (read <$> some parseDigit)

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

parseReportList :: Parser [Lib1.ReportCommand]
parseReportList = many (skipWhitespace *> (reportHouse <|> reportRoom <|> reportDevice))


parseCommand :: Parser Lib1.Command
parseCommand = parseFull $
  dump <|>
  add <|>
  remove <|>
  rename <|>
  set <|>
  control <|>
  schedule <|>
  report <|>
  simulate

parseFull :: Parser a -> Parser a
parseFull p = Parser $ \input ->
  case runParser p input of
    Left e -> Left e
    Right (result, rest) ->
      let rest' = dropWhile (== ' ') rest
      in if null rest'
           then Right (result, "")
           else Left ("Unconsumed input after command: '" ++ take 30 rest' ++ "'")

dump :: Parser Lib1.Command 
dump = (\_ _ _ -> Lib1.Dump Lib1.Examples)
  <$> parseKeyword "dump"
  <*> parseSpaces
  <*> parseKeyword "examples"



add :: Parser Lib1.Command
add = Lib1.Add <$> (addHouse <|> addRoom <|> addDevice)
addHouse :: Parser Lib1.AddCommand
addHouse = (\_ _ _ _ _ _ a -> Lib1.AddHouse a)
  <$> parseKeyword "add"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "house"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
addRoom :: Parser Lib1.AddCommand
addRoom = (\_ _ _ _ _ _ a _ _ _ _ _ b -> Lib1.AddRoom a b)
  <$> parseKeyword "add"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "room"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
addDevice :: Parser Lib1.AddCommand
addDevice = (\_ _ _ _ _ _ a _ _ _ _ _ b -> Lib1.AddDevice a b)
  <$> parseKeyword "add"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString


remove :: Parser Lib1.Command
remove = Lib1.Remove <$> (removeHouse <|> removeRoom <|> removeDevice)
removeHouse :: Parser Lib1.RemoveCommand
removeHouse = (\_ _ _ _ _ _ a-> Lib1.RemoveHouse a)
  <$> parseKeyword "remove"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "house"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
removeRoom :: Parser Lib1.RemoveCommand
removeRoom = (\_ _ _ _ _ _ a _ _ _ _ _ b -> Lib1.RemoveRoom a b)
  <$> parseKeyword "remove"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "room"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "from"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
removeDevice :: Parser Lib1.RemoveCommand
removeDevice = (\_ _ _ _ _ _ a _ _ _ _ _ b -> Lib1.RemoveDevice a b)
  <$> parseKeyword "remove"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "from"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString

rename :: Parser Lib1.Command
rename = Lib1.Rename <$> (renameHouse <|> renameRoom <|> renameDevice)
renameHouse :: Parser Lib1.RenameCommand
renameHouse = (\_ _ _ _ _ _ a _ _ _ _ _ b -> Lib1.RenameHouse a b)
  <$> parseKeyword "rename"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "house"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
renameRoom :: Parser Lib1.RenameCommand
renameRoom = (\_ _ _ _ _ _ a _ _ _ _ _ b -> Lib1.RenameRoom a b)
  <$> parseKeyword "rename"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "room"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
renameDevice :: Parser Lib1.RenameCommand
renameDevice = (\_ _ _ _ _ _ a _ _ _ _ _ b -> Lib1.RenameDevice a b)
  <$> parseKeyword "rename"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "device"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "to"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString  

set :: Parser Lib1.Command
set = Lib1.Set <$> (setBrightness <|> setTemperature <|> setState)
setBrightness :: Parser Lib1.SetCommand
setBrightness = (\_ _ _ a _ _ _ _ _ b -> Lib1.SetBrightness a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "brightness"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseDouble
setTemperature :: Parser Lib1.SetCommand
setTemperature = (\_ _ _ a _ _ _ _ _ b -> Lib1.SetTemperature a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "temperature"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseDouble
setState :: Parser Lib1.SetCommand
setState = (\_ _ _ a _ _ _ _ _ b -> Lib1.SetState a b)
  <$> parseKeyword "set"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "state"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseState

control :: Parser Lib1.Command
control = Lib1.Control <$> (turnOn <|> turnOff)
turnOn :: Parser Lib1.ControlCommand
turnOn = (\_ _ _ _ _ _ a -> Lib1.TurnOn a)
  <$> parseKeyword "turn"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "on"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
turnOff :: Parser Lib1.ControlCommand
turnOff = (\_ _ _ _ _ _ a -> Lib1.TurnOff a)
  <$> parseKeyword "turn"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseKeyword "off"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString

schedule :: Parser Lib1.Command
schedule = Lib1.Schedule <$> scheduleAt
scheduleAt :: Parser Lib1.ScheduleCommand
scheduleAt = (\_  _ _ a  _ _ b _ _ c -> Lib1.ScheduleAt a b c)
  <$> parseKeyword "schedule"
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseString
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseAction
  <*> parseSpaces
  <*> skipWhitespace
  <*> parseDouble

report :: Parser Lib1.Command
report =  Lib1.Report <$> (reportHouse <|> reportRoom <|> reportDevice)
reportHouse :: Parser Lib1.ReportCommand
reportHouse =
  (\_ _ _ _ _ _ name _ _ reports -> Lib1.ReportHouse name reports)
    <$> parseKeyword "report"
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseKeyword "house"
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseString
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseReportList
reportRoom :: Parser Lib1.ReportCommand
reportRoom =
  (\_ _ _ _ _ _ name _ _ reports -> Lib1.ReportRoom name reports)
    <$> parseKeyword "report"
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseKeyword "room"
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseString
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseReportList
reportDevice :: Parser Lib1.ReportCommand
reportDevice =
  (\_ _ _ _ _ _ name -> Lib1.ReportDevice name)
    <$> parseKeyword "report"
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseKeyword "device"
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseString

simulate :: Parser Lib1.Command
simulate = (\_ _ _ _ -> Lib1.Simulate Lib1.SimulateDay)
    <$> parseKeyword "simulate"
    <*> parseSpaces
    <*> skipWhitespace
    <*> parseKeyword "day"

-- | You can change the type to whatever needed. If your domain
-- does not have any state you have to make it up.
newtype State = State ()

-- Fix this accordingly
emptyState :: State
emptyState = State()

-- | Business/domain logic happens here.
-- This function makes your program actually usefull.
-- You may print if you want to print, you
-- may mutate state if needed but there must be
-- SINGLE atomically call in the function
-- You do not want to write/read files here.
execute :: TVar State -> Lib1.Command -> IO ()
execute _ _ = error "Implement me 1"

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request. It must run forever.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop c = do
  _ <- readChan c
  return $ error "Implement me 2"

-- | This function will be called periodically
-- and on programs' exit. File writes must be performed
-- through `Chan StorageOp`.
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save _ _ = return $ Left "Implement me 3"

-- | This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load _ _ = return $ Left "Implement me 4"
