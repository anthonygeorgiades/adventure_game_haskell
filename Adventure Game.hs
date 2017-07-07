import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String

type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("dorm_room", "out"), "bus_stop"),
    (("bus_stop", "in"), "bus"),
    (("bus_stop", "in"), "dorm_room"),
    (("bus", "out"), "classroom"),
    ]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("book", "dorm_room"),
    ("tv", "dorm_room"),
    ("alarm_clock", "dorm_room"),
    ("exam", "classroom"),
    ("myself", "dorm_room"),
    ("myself", "classroom"), 
    ("tv", "mounted"), 
    ]

type AlarmSet = Bool
alarmstatus = False

type Sleep = Bool
sleepstatus = False

type KnowledgeLevel = Int
knowledge = 0 

type World = (PathMap, LocationMap, AlarmSet, KnowledgeLevel, Response)
world :: IO (PathMap, LocationMap, AlarmSet, KnowledgeLevel, Response)
world = return (paths, locations, alarmstatus, knowledge, "")


main :: IO (String)
main = do
    putStrLn "\nWelcome to the Exam Adenture Game!\n"
    putStrLn instructions
    play_game $ return (paths, locations, "")
    return "Goodbye!"

instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "start              -- to start the game.\n" ++
    "in  out            -- to go in or out that direction.\n" ++
    "take object        -- to pick up an object.\n" ++
    "drop object        -- to put down an object.\n" ++
    "use object         -- to use an object.\n" ++
    "look               -- to look around you again.\n" ++
    "instructions       -- to see this message again.\n" ++
    "sleep              -- to get some rest before the exam).\n" ++
    "halt               -- to end the game and quit."

play_game :: IO (World) -> IO (World)
play_game world = do
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    if game_over locations
        then return ([], [], "")
        else do
            putStr "command> "
            command <- getLine
            if command == "quit"
                then return (paths, locations, "Quitting.")
                else  play_game $ return (do_command command paths locations)

--Game over when you take the exam
game_over :: LocationMap -> Bool
game_over locations =
    let my_location = get "myself" locations
        exam_location = get "exam" locations
        score_result = get score --Prints score from function below
    in my_location == "classroom" && exam_location == "holding" 


--Game over when the alarm is not set and you are in your dorm room and you are sleeping
game_over locations2 =
    let my_location = get "myself" locations
        alarm_status = get "alarm_clock" AlarmSet
        sleep_status = get "sleepstatus" Sleep
    in my_location == "drop" && alarm_status == False && sleep_status == True

paths :: PathMap
paths = [
    (("dorm_room", "out"), "bus_stop"),
    (("bus_stop", "in"), "bus"),
    (("bus_stop", "in"), "dorm_room"),
    (("bus", "out"), "classroom"),
    ]

can_move :: Location -> Direction -> Response
can_move "dorm_room" "out" _ locations = "You're going to the bus stop"
can_move "bus_stop" "in" _ locations = "You're getting on the bus"
can_move "bus" "out" _ locations = "You're going into class"
can_move from direction paths _ =
    elem (from, direction) keys 
    where (keys, _) = unzip paths

cannot_move_because :: Location -> Direction -> Response
cannot_move_because _ _ = "You can't go that way."

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "in" paths locations = go "in" paths locations
do_command "out" paths locations = go "out" paths locations
do_command "look" paths locations = look paths locations
do_command "sleep" paths locations = look paths locations
do_command "halt" paths locations = (paths, locations, "halt")
do_command "dump" paths locations =
    (paths, locations, "paths = " ++ show paths ++ "\nlocations = " ++ show locations)
do_command cmd paths locations = do_command_2 cmd paths locations

do_command_2 :: String -> PathMap -> LocationMap -> World
do_command_2 cmd paths locations
    | isPrefixOf "take " cmd =
          game_take (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "drop " cmd =
          game_drop (tail $ snd $ span isLetter cmd) paths locations
    | otherwise = (paths, locations, "I don't understand: " ++ cmd)

game_take :: Thing -> PathMap -> LocationMap -> World          
game_take thing paths locations =
    let here = get "myself" locations
        there = get thing locations
    in if here == there
       then (paths, (put thing "holding" locations), "OK, taken.")
       else if there == "holding"
            then (paths, locations, "You are already holding it.")
            else (paths, locations, "I don't see it here.")
        
game_drop :: Thing -> PathMap -> LocationMap -> World          
game_drop thing paths locations = 
    let here = get "myself" locations
        there = get thing locations
    in if there == "holding"
        then (paths, (put thing here locations), "OK, dropped.")
        else (paths, locations, "You aren't holding it.")

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
    let my_location = get "myself" locations
    if can_move my_location direction paths locations
        then do
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)
        else (paths, locations, cannot_move_because my_location direction)

add_knowledge :: String -> PathMap -> LocationMap -> World
add_knowledge direction paths locations =
    if get "myself" locations == "dorm_room" &&
       get "book" locations == "dorm_room" &&
       get "book" locations == "holding"
        then(PathMap, LocationMap, AlarmSet, put KnowledgeLevel+1, response "Your knowledge level went up!")
           else go direction paths locations 

set_alarm :: Bool -> PathMap -> LocationMap -> World
set_alarm direction paths locations =
    if get "myself" locations == "dorm_room" &&
       get "alarm_clock" locations == "dorm_room" 
        then(PathMap, LocationMap, AlarmSet=True, KnowledgeLevel, response "Your alarm is now set")
           else go direction paths locations 

look :: PathMap -> LocationMap -> World
look paths locations =
    if things == []
        then (paths, locations, describe my_location locations)
        else (paths, locations, describe my_location locations ++ "\n\n" ++ things)
    where my_location = get "myself" locations
          things = items_here locations

score :: PathMap -> LocationMap -> World
score paths locations =
    case get "myself" locations of
        "classroom" -> (paths,
                   put "myself" "exam" locations && KnowledgeLevel < 3 
                   "'Sorry, you flunked the exam.")
        "classroom" -> (paths,
                   put "myself" "exam" locations && KnowledgeLevel == 3
                   "'Sorry, you flunked the exam.")
        "classroom" -> (paths,
                   put "myself" "exam" locations && KnowledgeLevel == 3
                   "Wow, you aced the exam! I didn''t think that was possible!")
        _ -> (paths, locations, "I cannot give you a score")

inventory :: LocationMap -> Response
inventory locations =
    let my_stuff = [thing | (thing, "holding") <- locations]
    in if my_stuff == []
        then "You aren't holding anything."
        else intercalate ", " my_stuff

items_here :: LocationMap -> Response
items_here locations =
    let here = get "myself" locations
        things = ["There is a " ++ thing ++ " here." |
                  (thing, place) <- locations, place == here, thing /= "myself"]
    in intercalate "\n" things

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "myself" locations
        exam_location = get "exam" locations
        knowledge = get "knowledge" KnowledgeLevel
    in describe_helper here spider_status ruby_location  locations 

    describe :: Location -> LocationMap -> String

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "myself" "exam" "holding" locations = description "exam1"
describe_helper "myself" "dorm_room" "sleeping" locations AlarmSet is false = description "sleeping1"
describe_helper "myself" "dorm_room" "sleeping" locations AlarmSet is true = description "sleeping2"
describe_helper "myself" "book" "holding" locations = description "studying1"
describe_helper "myself" "exam" "score" locations = description "score1"

describe_helper here _ _ locations = description here

description "score1" = "Congratulations!!  You took the exam! What is your score?? "
description "sleeping1" = "Zzzzzzz!!  You are sleeping and not studying and your alarm is not set"
description "sleeping2" = "Zzzzzzz!!  You are sleeping and not studying BUT your alarm is set!"
description "exam1" = "You are taking the exam now!"

description someplace = someplace ++ "What are you doing! There is an exam. Sleep or study or do something!"

