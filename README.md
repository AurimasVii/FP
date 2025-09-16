# fp-2025

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

## Chosen domain
### Smart Home
1. Main entities:
    - House
    - Room
    - Device
    - Rule
    - Status
2. Operations:
    - `add house <name>` – Create a new house.  
    - `add room <name> to <house>` – Create a room inside a house.  
    - `add device <name> to <room>` – Add a device to a room (recursive if adding to a smart hub).  
    - `remove house/room/device <name>` – Remove an entity and its sub-elements.  
    - `rename house/room/device <oldName> to <newName>` – Rename an entity.  
    - `set device <name> state <value>` – Change the state of a device.  

    - `turn on <device>` / `turn off <device>` – Control device power.  
    - `set <device> brightness <value>` – Adjust brightness of a device.  
    - `set <device> temperature <value>` – Adjust temperature of a device.  
    - `schedule <device> <action> at <time>` – Create automation for a device.  
    - `simulate day` – Execute scheduled actions for all devices recursively.  
    - `report room <name>` – Show the state of all devices in a room.  
    - `report house <name>` – Recursively report the state of all rooms and devices in a house.  
    - `dump examples` – Output example commands to demonstrate the DSL syntax.
3. Examples
    ``` 
        1. add house MyHome
         Creates a new house called "MyHome".

        2. add room Kitchen to MyHome
           # Adds a room "Kitchen" inside "MyHome".
        
        3. add device Lamp to Kitchen
           # Adds a simple device "Lamp" in the "Kitchen".
        
        4. add device SmartHub to Kitchen
           add device CoffeeMaker to SmartHub
           add device Toaster to SmartHub
           # Demonstrates recursion: "SmartHub" contains sub-devices "CoffeeMaker" and "Toaster".
        
        5. turn on Lamp
           # Turns on a single device.
        
        6. report house MyHome
           # Recursively shows all rooms and devices in "MyHome", including devices inside SmartHub.
        
    ```
## BNF
```
   <command> ::= 
     <add_command> 
   | <remove_command>
   | <set_command> 
   | <rename_command> 
   | <control_command> 
   | <schedule_command> 
   | <report_command> 
   | <simulate_command> 
   | "dump examples"

   <add_command> ::= 
                     "add house " <house_name> 
                  |  "add a room " <room_name> " to " <house_name> 
                  |  "add a device " <device_name> " to " <room_name>

   <remove_command> ::= 
                     "remove house " <house_name> 
                  |  "remove room " <room_name> " from house " <house_name> 
                  |  "remove device " <device_name> " from room " <room_name>

   <set_command> ::= 
                     "set " <device_name> " brightness " <value> 
                  |  "set " <device_name> " temperature " <value> 
                  |  "set " <device_name> " state " <state>

   <rename_command> ::= 
                     "rename house " <old_name> " to " <new_name> 
                  |  "rename room " <old_name> " to " <new_name> 
                  |  "rename device " <old_name> " to " <new_name>

   <control_command> ::= 
                     "turn on " <device_name> 
                  |  "turn off " <device_name>

   <schedule_command> ::= "schedule " <device_name> <action> " at " <value>

   <report_command> ::= 
                     "report room " <room_name> 
                  |  "report house " <house_name>

   <simulate_command> ::= "simulate day"

   <state> ::= "On" | "Off"

   <house_name> ::= <identifier>
   <room_name> ::= <identifier>
   <device_name> ::= <identifier>
   <old_name> ::= <identifier>
   <new_name> ::= <identifier> 

   <identifier> ::= <uppercase_letter> <lowercase_letter>* <numeric>*
   <uppercase_letter> ::= [A-Z]
   <lowercase_letter> ::= [a-z]
   <numeric> ::= [0-9]
   <value> ::= <numeric> | <numeric> "." <numeric> <numeric>*
   <action> ::= " turn on" | " turn off" | " set brightness" | " set temperature"
```