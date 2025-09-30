module Lib1( examples, Command(..), Dumpable(..)) where

data Dumpable = Examples
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed
data Command = Dump Dumpable 
             | Add AddCommand
             | Remove RemoveCommand 
             | Set SetCommand
             | Rename RenameCommand
             | Control ControlCommand
             | Schedule ScheduleCommand
             | Report ReportCommand
             | Simulate SimulateCommand

instance Show Command where
  show (Dump d)         = show d
  show (Add a)          = show a
  show (Remove r)       = show r
  show (Set s)          = show s
  show (Rename r)       = show r
  show (Control c)      = show c
  show (Schedule sc)    = show sc
  show (Report rc)      = show rc
  show (Simulate s)     = show s 

data AddCommand = AddHouse String
                | AddRoom String String
                | AddDevice String String

instance Show AddCommand where
  show (AddHouse h)     = "add house " ++ h
  show (AddRoom r h)    = "add room " ++ r ++ " to " ++ h
  show (AddDevice d r)  = "add device " ++ d ++ " to " ++ r  

data RemoveCommand = RemoveHouse String
                   | RemoveRoom String String
                   | RemoveDevice String String

instance Show RemoveCommand where
  show (RemoveHouse h)      = "remove house " ++ h
  show (RemoveRoom r h)     = "remove room " ++ r ++ " from house " ++ h
  show (RemoveDevice d r)   = "remove device " ++ d ++ " from room " ++ r

data SetCommand = SetBrightness String Double
                | SetTemperature String Double
                | SetState String State

instance Show SetCommand where
  show (SetBrightness d v)    = "set " ++ d ++ " brightness to " ++ show v
  show (SetTemperature d v)   = "set " ++ d ++ " temperature to " ++ show v
  show (SetState d s)         = "set " ++ d ++ " state to " ++ show s


data RenameCommand = RenameHouse String String
                   | RenameRoom String String
                   | RenameDevice String String

instance Show RenameCommand where
  show (RenameHouse oh nh)      = "rename house " ++ oh ++ " to " ++ nh
  show (RenameRoom orn nr)       = "rename room " ++ orn ++ " to " ++ nr
  show (RenameDevice od nd)     = "rename device " ++ od ++ " to " ++ nd

data ControlCommand = TurnOn String
                    | TurnOff String

instance Show ControlCommand where
  show (TurnOn d)   = "turn on " ++ d
  show (TurnOff d)  = "turn off " ++ d

data ScheduleCommand = ScheduleAt String Action Double

instance Show ScheduleCommand where
  show (ScheduleAt d a v) = "schedule " ++ d ++ show a ++ " at " ++ show v

data ReportCommand = ReportHouse String
                   | ReportRoom String

instance Show ReportCommand where
  show (ReportHouse h) = "report house " ++ h
  show (ReportRoom r)  = "report room " ++ r 

data SimulateCommand = SimulateDay

instance Show SimulateCommand where
  show SimulateDay = "simulate day"

data State = On | Off

instance Show State where
  show On   = "On"
  show Off  = "Off"

data Action = TurnOnDevice 
            | TurnOffDevice 
            | SetBrightnessLevel 
            | SetTemperatureLevel

instance Show Action where
  show TurnOnDevice         = " turn on"
  show TurnOffDevice        = " turn off"
  show SetBrightnessLevel   = " set brightness"
  show SetTemperatureLevel  = " set temperature"

examples :: [Command]
examples =
    [ Add (AddHouse "GreenVilla")
    , Add (AddRoom "Kitchen" "GreenVilla")
    , Add (AddDevice "SmartHub" "Kitchen")
    , Add (AddDevice "CoffeeMaker" "SmartHub")
    , Report (ReportHouse "GreenVilla")
    , Schedule (ScheduleAt "Heater" SetTemperatureLevel 21.5)
    , Dump Examples 
    ]
