module Lib1(
    examples
  , Command(..)
  , Dumpable(..)
  , AddCommand(..)
  , RemoveCommand(..)
  , SetCommand(..)
  , RenameCommand(..)
  , ControlCommand(..)
  , ScheduleCommand(..)
  , ReportCommand(..)
  , SimulateCommand(..)
  , State(..)
  , Action(..)
  ) where


data Dumpable = Examples
  deriving Show

data Command = Dump Dumpable 
             | Add AddCommand
             | Remove RemoveCommand 
             | Set SetCommand
             | Rename RenameCommand
             | Control ControlCommand
             | Schedule ScheduleCommand
             | Report ReportCommand
             | Simulate SimulateCommand
  deriving Show 


data AddCommand = AddHouse String
                | AddRoom String String
                | AddDevice String String
  deriving Show


data RemoveCommand = RemoveHouse String
                   | RemoveRoom String String
                   | RemoveDevice String String
  deriving Show


data SetCommand = SetBrightness String Double
                | SetTemperature String Double
                | SetState String State
  deriving Show



data RenameCommand = RenameHouse String String
                   | RenameRoom String String
                   | RenameDevice String String
  deriving Show


data ControlCommand = TurnOn String
                    | TurnOff String
  deriving Show


data ScheduleCommand = ScheduleAt String Action Double
  deriving Show


data ReportCommand = ReportHouse String [ReportCommand] | ReportRoom String[ReportCommand] | ReportDevice String                    
  deriving Show

data SimulateCommand = SimulateDay
  deriving Show

data State = On | Off
  deriving Show


data Action = TurnOnDevice 
            | TurnOffDevice 
            | SetBrightnessLevel 
            | SetTemperatureLevel
  deriving Show


examples :: [Command]
examples =
    [ Add (AddHouse "GreenVilla")
    , Add (AddRoom "Kitchen" "GreenVilla")
    , Add (AddDevice "SmartHub" "Kitchen")
    , Add (AddDevice "CoffeeMaker" "SmartHub")
    , Report (ReportHouse "GreenVilla"
                [ ReportRoom "Kitchen"
                    [ ReportDevice "SmartHub"
                    , ReportDevice "CoffeeMaker"
                    ]
                ]
             )
    , Schedule (ScheduleAt "Heater" SetTemperatureLevel 21.5)
    , Dump Examples 
    ]

