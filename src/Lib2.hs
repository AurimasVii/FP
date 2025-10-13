{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1
import Data.List (stripPrefix)
import Data.Char (isAlpha, isDigit)

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

parseDigit :: Parser Char
parseDigit [] = Left " A digit is expected but got empty input"
parseDigit (h:t)
  | isDigit h = Right (h, t)
  | otherwise = Left $ " A digit is expected, but got: " ++ [h]

parseChar :: Parser Char
parseChar [] = Left " A letter is expected but got empty input"
parseChar (h:t)
  | isAlpha h = Right (h, t)
  | otherwise = Left $ " A letter is expected, but got: " ++ [h]

skipWhitespace :: Parser String
skipWhitespace input = Right (spaces, rest)
  where (spaces, rest) = span (== ' ') input

parseNumber :: Parser Int
parseNumber input =
  case many1 parseDigit input of
    Left e -> Left e
    Right (digits, rest) -> Right (read digits, rest)

parseLetter :: Parser Char
parseLetter = parseChar

parseIdentifier :: Parser String
parseIdentifier = parseString

parseState :: Parser Lib1.State
parseState = orElse (using (keyword "On") (const Lib1.On)) 
                    (using (keyword "Off") (const Lib1.Off))

parseAction :: Parser Lib1.Action
parseAction = orElse (using (keyword "turn on") (const Lib1.TurnOnDevice))
              (orElse (using (keyword "turn off") (const Lib1.TurnOffDevice))
              (orElse (using (keyword "set brightness") (const Lib1.SetBrightnessLevel))
                      (using (keyword "set temperature") (const Lib1.SetTemperatureLevel))))

parseString :: Parser String
parseString input =
  case many1 parseChar input of
    Left e -> Left e
    Right (chars, rest) -> Right (chars, rest)
parseDouble :: Parser Double
parseDouble input =
  case many1 parseDigit input of
    Left e -> Left e
    Right (intPart, rest1) ->
      case keyword "." rest1 of
        Left _ -> Right (read intPart, rest1)
        Right (_, rest2) ->
          case many1 parseDigit rest2 of
            Left e -> Left e
            Right (fracPart, rest3) ->
              let numStr = intPart ++ "." ++ fracPart
              in Right (read numStr, rest3)


many :: Parser a -> Parser [a]
many p input
  = case p input of
      Left _ -> Right ([], input)
      Right (v, rest)
        -> case many p rest of
             Right (vs, rest') -> Right (v : vs, rest')
             Left e -> Left e

many1 :: Parser a -> Parser [a]
many1 p input
  = case p input of
      Left e -> Left e
      Right (v, rest)
        -> case many p rest of
             Right (vs, rest') -> Right (v : vs, rest')
             Left e2 -> Left e2

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input
  = case p1 input of
      Right r1 -> Right r1
      Left e1
        -> case p2 input of
             Right r2 -> Right r2
             Left e2 -> Left (e1 ++ e2)

keyword :: String -> Parser String
keyword prefix input =
  let trimmed = dropWhile (== ' ') input
  in case stripPrefix prefix trimmed of
       Just rest -> Right (prefix, rest)
       Nothing   -> Left ("Expected '" ++ prefix ++ "', but got: " ++ take 20 trimmed)


using :: Parser a -> (a -> b) -> Parser b
using p f input =
  case p input of
    Left e -> Left e
    Right (v, rest) -> Right (f v, rest)
      
and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input =
  case p1 input of
    Left e1 -> Left e1
    Right (v1, rest1) ->
      case p2 rest1 of
        Left e2 -> Left e2
        Right (v2, rest2) -> Right ((v1, v2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 input = 
  case p1 input of
    Left e1 -> Left e1 
    Right (v1, r1) ->
      case p2 r1 of 
        Left e2 -> Left e2
        Right (v2, r2) ->
          case p3 r2 of
            Left e3 -> Left e3 
            Right (v3, r3) -> Right((v1, v2, v3), r3)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 p1 p2 p3 p4 input =
  case and3 p1 p2 p3 input of
    Left e -> Left e
    Right ((v1, v2, v3), r) ->
      case p4 r of
        Left e -> Left e
        Right (v4, r') -> Right ((v1, v2, v3, v4), r')

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a, b, c, d, e)
and5 p1 p2 p3 p4 p5 input =
  case and4 p1 p2 p3 p4 input of
    Left e -> Left e
    Right ((v1, v2, v3, v4), rest) ->
      case p5 rest of
        Left e -> Left e
        Right (v5, rest') -> Right ((v1, v2, v3, v4, v5), rest')

parseDumpCommand :: Parser Lib1.Command
parseDumpCommand =
  and2 (keyword "dump") (and2 skipWhitespace (keyword "examples"))
    `using` (\(_, (_, _)) -> Lib1.Dump Lib1.Examples)


parseAddHouse :: Parser Lib1.Command
parseAddHouse =
  and3 (keyword "add") (and2 skipWhitespace (keyword "house")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, name)) -> Lib1.Add (Lib1.AddHouse name))
parseAddRoom :: Parser Lib1.Command
parseAddRoom =
  and5 (keyword "add") (and2 skipWhitespace (keyword "room")) (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "to")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, roomName), (_, _), (_, houseName)) -> Lib1.Add (Lib1.AddRoom roomName houseName))
parseAddDevice :: Parser Lib1.Command
parseAddDevice = 
  and5 (keyword "add") (and2 skipWhitespace (keyword "device")) (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "to")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, deviceName), (_, _), (_, roomName)) -> Lib1.Add (Lib1.AddDevice deviceName roomName))

parseAddCommand :: Parser Lib1.Command
parseAddCommand =
  parseAddHouse
  `orElse` parseAddRoom
  `orElse` parseAddDevice


parseRemoveHouse :: Parser Lib1.Command
parseRemoveHouse =
  and3 (keyword "remove") (and2 skipWhitespace (keyword "house")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, houseName)) -> Lib1.Remove (Lib1.RemoveHouse houseName))
parseRemoveRoom :: Parser Lib1.Command
parseRemoveRoom = 
  and5 (keyword "remove") (and2 skipWhitespace (keyword "room")) (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "from")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, roomName), (_, _), (_, houseName)) -> Lib1.Remove (Lib1.RemoveRoom roomName houseName))
parseRemoveDevice :: Parser Lib1.Command
parseRemoveDevice =
  and5 (keyword "remove") (and2 skipWhitespace (keyword "device")) (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "from")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, deviceName), (_, _), (_, roomName)) -> Lib1.Remove (Lib1.RemoveDevice deviceName roomName))
parseRemoveCommand :: Parser Lib1.Command
parseRemoveCommand = 
  parseRemoveHouse
  `orElse` parseRemoveRoom
  `orElse` parseRemoveDevice

parseRenameHouse :: Parser Lib1.Command
parseRenameHouse = 
  and5 (keyword "rename") (and2 skipWhitespace (keyword "house")) (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "to")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, oldName), (_, _), (_, newName)) -> Lib1.Rename (Lib1.RenameHouse oldName newName))
parseRenameRoom :: Parser Lib1.Command
parseRenameRoom =
  and5 (keyword "rename") (and2 skipWhitespace (keyword "room")) (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "to")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, oldName), (_, _), (_, newName)) -> Lib1.Rename (Lib1.RenameRoom oldName newName))
parseRenameDevice :: Parser Lib1.Command
parseRenameDevice =
  and5 (keyword "rename") (and2 skipWhitespace (keyword "device")) (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "to")) (and2 skipWhitespace parseString)
    `using` (\(_, (_, _), (_, oldName), (_, _), (_, newName)) -> Lib1.Rename (Lib1.RenameDevice oldName newName))
parseRenameCommand :: Parser Lib1.Command
parseRenameCommand =
  parseRenameHouse
  `orElse` parseRenameRoom
  `orElse` parseRenameDevice


parseSetBrightness :: Parser Lib1.Command
parseSetBrightness =
  and4 (keyword "set") (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "brightness")) (and2 skipWhitespace parseDouble)
    `using` (\(_, (_, deviceName), (_, _), (_, brightness)) -> Lib1.Set (Lib1.SetBrightness deviceName brightness))

parseSetTemperature :: Parser Lib1.Command
parseSetTemperature =
  and4 (keyword "set") (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "temperature")) (and2 skipWhitespace parseDouble)
    `using` (\(_, (_, deviceName), (_, _), (_, temperature)) -> Lib1.Set (Lib1.SetTemperature deviceName temperature))

parseSetState :: Parser Lib1.Command
parseSetState =
  and4 (keyword "set") (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "state")) (and2 skipWhitespace parseState)
    `using` (\(_, (_, deviceName), (_, _), (_, state)) -> Lib1.Set (Lib1.SetState deviceName state))


parseSetCommand :: Parser Lib1.Command
parseSetCommand =
  parseSetBrightness
  `orElse` parseSetTemperature
  `orElse` parseSetState

parseTurnOn :: Parser Lib1.Command
parseTurnOn =
  and2 (keyword "turn on") (and2 skipWhitespace parseString)
    `using` (\(_, (_, deviceName)) -> Lib1.Control (Lib1.TurnOn deviceName))
parseTurnOff :: Parser Lib1.Command
parseTurnOff =
  and2 (keyword "turn off") (and2 skipWhitespace parseString)
    `using` (\(_, (_, deviceName)) -> Lib1.Control (Lib1.TurnOff deviceName))
parseControlCommand :: Parser Lib1.Command
parseControlCommand =
  parseTurnOn
  `orElse` parseTurnOff

parseScheduleCommand :: Parser Lib1.Command
parseScheduleCommand = 
  and5 (keyword "schedule device") (and2 skipWhitespace parseString) (and2 skipWhitespace (keyword "to")) (and2 skipWhitespace parseAction) (and3 skipWhitespace (keyword "at") (and2 skipWhitespace parseDouble))
    `using` (\(_, (_, deviceName), (_, _), (_, action), (_, _, (_, time))) -> Lib1.Schedule (Lib1.ScheduleAt deviceName action time))

parseReportList :: Parser [Lib1.ReportCommand]
parseReportList input =
  case parseReportCommand input of
    Left _ -> Right([], input)
    Right (cmd, r) ->
      case cmd of
        Lib1.Report rc ->
          case parseReportList r of
            Left _ -> Right([rc], r)
            Right(mrc, r') -> Right (rc: mrc, r')
        _ -> Left "Expected a report command"
parseReportHouse :: Parser Lib1.Command
parseReportHouse =
  and3 (keyword "report house") (and2 skipWhitespace parseString) parseReportList
    `using` (\(_, (_, houseName), reportList) -> Lib1.Report (Lib1.ReportHouse houseName reportList))

parseReportRoom :: Parser Lib1.Command
parseReportRoom =
  and3 (keyword "report room") (and2 skipWhitespace parseString) parseReportList
    `using` (\(_, (_, roomName), reportList) -> Lib1.Report (Lib1.ReportRoom roomName reportList))

parseReportDevice :: Parser Lib1.Command
parseReportDevice =
  and2 (keyword "report device") (and2 skipWhitespace parseString)
    `using` (\(_, (_, deviceName)) -> Lib1.Report (Lib1.ReportDevice deviceName))
parseReportCommand :: Parser Lib1.Command
parseReportCommand =
  parseReportHouse
  `orElse` parseReportRoom
  `orElse` parseReportDevice

parseSimulateCommand :: Parser Lib1.Command
parseSimulateCommand =
  and2 (keyword "simulate") (and2 skipWhitespace (keyword "day"))
    `using` (\(_, (_, _)) -> Lib1.Simulate Lib1.SimulateDay)
-- | Parses user's input.
-- The function must be implemented and must have tests.
parseCommand :: Parser Lib1.Command
parseCommand  = 
  parseDumpCommand
  `orElse` parseAddCommand
  `orElse` parseRemoveCommand
  `orElse` parseRenameCommand
  `orElse` parseSetCommand
  `orElse` parseControlCommand
  `orElse` parseScheduleCommand
  `orElse` parseReportCommand
  `orElse` parseSimulateCommand


process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

-- | You have to make your Command an instance of ToCliCommand class.
-- Please remove all custom Show instances of Command ADT and
-- use "deriving Show" only.
instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"
  toCliCommand (Lib1.Add (Lib1.AddHouse name)) = "add house " ++ name
  toCliCommand (Lib1.Add (Lib1.AddRoom roomName houseName)) = "add room " ++ roomName ++ " to " ++ houseName
  toCliCommand (Lib1.Add (Lib1.AddDevice deviceName roomName)) = "add device " ++ deviceName ++ " to " ++ roomName
  toCliCommand (Lib1.Remove (Lib1.RemoveHouse name)) = "remove house " ++ name
  toCliCommand (Lib1.Remove (Lib1.RemoveRoom roomName houseName)) = "remove room " ++ roomName ++ " from " ++ houseName
  toCliCommand (Lib1.Remove (Lib1.RemoveDevice deviceName roomName)) = "remove device " ++ deviceName ++ " from " ++ roomName
  toCliCommand (Lib1.Rename (Lib1.RenameHouse oldName newName)) = "rename house " ++ oldName ++ " to " ++ newName
  toCliCommand (Lib1.Rename (Lib1.RenameRoom oldName newName)) = "rename room " ++ oldName ++ " to " ++ newName
  toCliCommand (Lib1.Rename (Lib1.RenameDevice oldName newName)) = "rename device " ++ oldName ++ " to " ++ newName
  toCliCommand (Lib1.Set (Lib1.SetBrightness deviceName brightness)) = "set device " ++ deviceName ++ " brightness to " ++ show brightness
  toCliCommand (Lib1.Set (Lib1.SetTemperature deviceName temperature)) = "set device " ++ deviceName ++ " temperature to " ++ show temperature
  toCliCommand (Lib1.Set (Lib1.SetState deviceName state)) = "set device " ++ deviceName ++ " state to " ++ case state of { Lib1.On -> "on"; Lib1.Off -> "off" }
  toCliCommand (Lib1.Control (Lib1.TurnOn deviceName)) = "turn on " ++ deviceName
  toCliCommand (Lib1.Control (Lib1.TurnOff deviceName)) = "turn off " ++ deviceName
  toCliCommand (Lib1.Schedule (Lib1.ScheduleAt deviceName action time)) = "schedule device " ++ deviceName ++ " to " ++ case action of 
    { Lib1.TurnOnDevice -> "turn on"
    ; Lib1.TurnOffDevice -> "turn off" 
    ; Lib1.SetBrightnessLevel -> "set brightness"
    ; Lib1.SetTemperatureLevel -> "set temperature"
    } ++ " at " ++ show time
  toCliCommand (Lib1.Report (Lib1.ReportHouse houseName reportList)) = "report house " ++ houseName ++ concatMap (\cmd -> " " ++ toCliCommand (Lib1.Report cmd)) reportList
  toCliCommand (Lib1.Report (Lib1.ReportRoom roomName reportList)) = "report room " ++ roomName ++ concatMap (\cmd -> " " ++ toCliCommand (Lib1.Report cmd)) reportList
  toCliCommand (Lib1.Report (Lib1.ReportDevice deviceName)) = "report device " ++ deviceName
  toCliCommand (Lib1.Simulate Lib1.SimulateDay) = "simulate day"

-- | You have to make your Command an instance of Eq class.
-- Usage of "deriving Eq" is forbidden.
instance Eq Lib1.State where
  Lib1.On == Lib1.On = True
  Lib1.Off == Lib1.Off = True
  _ == _ = False

instance Eq Lib1.Action where  
  Lib1.TurnOnDevice == Lib1.TurnOnDevice = True
  Lib1.TurnOffDevice == Lib1.TurnOffDevice = True
  Lib1.SetBrightnessLevel == Lib1.SetBrightnessLevel = True
  Lib1.SetTemperatureLevel == Lib1.SetTemperatureLevel = True
  _ == _ = False

instance Eq Lib1.ReportCommand where
  Lib1.ReportHouse h1 rs1 == Lib1.ReportHouse h2 rs2 = h1 == h2 && rs1 == rs2
  Lib1.ReportRoom r1 rs1 == Lib1.ReportRoom r2 rs2 = r1 == r2 && rs1 == rs2  
  Lib1.ReportDevice d1 == Lib1.ReportDevice d2 = d1 == d2
  _ == _ = False

instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  Lib1.Dump Lib1.Examples == Lib1.Dump Lib1.Examples = True
  Lib1.Add (Lib1.AddHouse name1) == Lib1.Add (Lib1.AddHouse name2) = name1 == name2
  Lib1.Add (Lib1.AddRoom roomName1 houseName1) == Lib1.Add (Lib1.AddRoom roomName2 houseName2) = roomName1 == roomName2 && houseName1 == houseName2
  Lib1.Add (Lib1.AddDevice deviceName1 roomName1) == Lib1.Add (Lib1.AddDevice deviceName2 roomName2) = deviceName1 == deviceName2 && roomName1 == roomName2
  Lib1.Remove (Lib1.RemoveHouse name1) == Lib1.Remove (Lib1.RemoveHouse name2) = name1 == name2
  Lib1.Remove (Lib1.RemoveRoom roomName1 houseName1) == Lib1.Remove (Lib1.RemoveRoom roomName2 houseName2) = roomName1 == roomName2 && houseName1 == houseName2
  Lib1.Remove (Lib1.RemoveDevice deviceName1 roomName1) == Lib1.Remove (Lib1.RemoveDevice deviceName2 roomName2) = deviceName1 == deviceName2 && roomName1 == roomName2
  Lib1.Rename (Lib1.RenameHouse oldName1 newName1) == Lib1.Rename (Lib1.RenameHouse oldName2 newName2) = oldName1 == oldName2 && newName1 == newName2
  Lib1.Rename (Lib1.RenameRoom oldName1 newName1) == Lib1.Rename (Lib1.RenameRoom oldName2 newName2) = oldName1 == oldName2 && newName1 == newName2
  Lib1.Rename (Lib1.RenameDevice oldName1 newName1) == Lib1.Rename (Lib1.RenameDevice oldName2 newName2) = oldName1 == oldName2 && newName1 == newName2
  Lib1.Set (Lib1.SetBrightness d1 b1) == Lib1.Set (Lib1.SetBrightness d2 b2)
    = d1 == d2 && b1 == b2
  Lib1.Set (Lib1.SetTemperature d1 t1) == Lib1.Set (Lib1.SetTemperature d2 t2)
    = d1 == d2 && t1 == t2
  Lib1.Set (Lib1.SetState d1 Lib1.On)  == Lib1.Set (Lib1.SetState d2 Lib1.On)
    = d1 == d2
  Lib1.Set (Lib1.SetState d1 Lib1.Off) == Lib1.Set (Lib1.SetState d2 Lib1.Off)
    = d1 == d2
  Lib1.Control (Lib1.TurnOn deviceName1) == Lib1.Control (Lib1.TurnOn deviceName2) = deviceName1 == deviceName2
  Lib1.Control (Lib1.TurnOff deviceName1) == Lib1.Control (Lib1.TurnOff deviceName2) = deviceName1 == deviceName2
  Lib1.Schedule (Lib1.ScheduleAt d1 Lib1.TurnOnDevice t1)
    == Lib1.Schedule (Lib1.ScheduleAt d2 Lib1.TurnOnDevice t2)
    = d1 == d2 && t1 == t2
  Lib1.Schedule (Lib1.ScheduleAt d1 Lib1.TurnOffDevice t1)
    == Lib1.Schedule (Lib1.ScheduleAt d2 Lib1.TurnOffDevice t2)
    = d1 == d2 && t1 == t2
  Lib1.Schedule (Lib1.ScheduleAt d1 Lib1.SetBrightnessLevel t1)
    == Lib1.Schedule (Lib1.ScheduleAt d2 Lib1.SetBrightnessLevel t2)
    = d1 == d2 && t1 == t2
  Lib1.Schedule (Lib1.ScheduleAt d1 Lib1.SetTemperatureLevel t1)
    == Lib1.Schedule (Lib1.ScheduleAt d2 Lib1.SetTemperatureLevel t2)
    = d1 == d2 && t1 == t2
  Lib1.Report (Lib1.ReportHouse h1 rs1)
    == Lib1.Report (Lib1.ReportHouse h2 rs2)
    = h1 == h2 && and (zipWith (==) (map Lib1.Report rs1) (map Lib1.Report rs2))
  Lib1.Report (Lib1.ReportRoom r1 rs1)
    == Lib1.Report (Lib1.ReportRoom r2 rs2)
    = r1 == r2 && and (zipWith (==) (map Lib1.Report rs1) (map Lib1.Report rs2))
  Lib1.Report (Lib1.ReportDevice d1)
    == Lib1.Report (Lib1.ReportDevice d2)
    = d1 == d2
  Lib1.Simulate Lib1.SimulateDay == Lib1.Simulate Lib1.SimulateDay = True
  _ == _ = False
