
import Control.Monad
import Data.Array.IO
import Data.List
import System.Console.ANSI
import System.IO
import System.Random


-- setting the stage---

type Position = (Int, Int)

data GameState = Playing { seeker :: Position, message :: String, over :: Bool }
                deriving (Eq)
data Item = Food { representation :: Char, position :: Position }
            deriving (Eq)

type Level = [Item]

-- defining commands and keys ---
data Command = MoveLeft
                | MoveDown
                | MoveUp
                | MoveRight
                | Quit
                | Unknown
                deriving (Eq)

parseInput :: [Char] -> [Command]
parseInput chars = map parseCommand chars

parseCommand :: Char -> Command
parseCommand 'q' = Quit
parseCommand 'h' = MoveLeft
parseCommand 'j' = MoveDown
parseCommand 'u' = MoveUp
parseCommand 'k' = MoveRight
parseCommand _ = Unknown


--  checking if the seeker found the food --
itemAt :: Position -> [Item] -> Maybe Item
itemAt pos = find (\ item -> (position item) == pos)

moveSeeker :: Level -> (Int, Int) -> GameState -> GameState
moveSeeker level (rowDelta, colDelta) curState =
    let (row, col) = seeker curState in
    let newR = (row + rowDelta, col + colDelta) in
    let itemInTheWay = itemAt newR in
    case itemAt newR level of
        Just (Food _ _) -> curState { message = "You found food!", over = True }
        Nothing -> curState { seeker = newR, message = "" }


advance :: Level -> GameState -> Command -> GameState
advance level state MoveLeft = moveSeeker level (0, -1) state
advance level state MoveUp = moveSeeker level (-1, 0) state
advance level state MoveDown = moveSeeker level (1, 0) state
advance level state MoveRight = moveSeeker level (0, 1) state
advance _ state Quit = state { message = "Goodbye!", over = True }
advance _ state _ = state


playing seekerPosition = Playing { seeker = seekerPosition, message = "", over = False }

-- running the code till seeker finds the food---
playGame :: Level -> [Char] -> GameState -> [GameState]
playGame level userInput initState = takeThrough over $
        scanl (advance level) initState $
        parseInput userInput

--
takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough p (x:xs)
                | not (p x) = x : takeThrough p xs
                | otherwise = [x]


transitions :: [a] -> [(a, a)]
transitions list =  zip  ([head list] ++ list) list


-- initializing screen with seeker and food ---
initScreen level Playing {seeker = seeker} = do
    hSetEcho stdin False
    clearScreen
    drawR seeker
    mapM_ drawItem level 

drawItem (Food representation position) = draw representation position


draw char (row, col) = do
    setCursorPosition row col
    putChar char

drawR = draw '#'
clear = draw ' '

clearState Playing { seeker = seekerPosition } = do
    clear seekerPosition
    setCursorPosition 26 0
    clearLine

drawState Playing { seeker = seekerPosition, message = message } = do
    drawR seekerPosition
    setCursorPosition 26 0
    putStr message

updateScreen (oldState, newState) = do
    clearState oldState
    drawState newState

-- generating random position for seeker and food ---
takeRandom count range = do
    g <- newStdGen
    return $ take count $ randomRs range g

takeRandomPositions count = do
    randomRows <- takeRandom count (0, 25)
    randomCols <- takeRandom count (0, 80)
    return $ zip randomRows randomCols


generateLevel = do
        [foodChar] <- takeRandom 1 ('A','Z')
        [foodPos] <- takeRandomPositions 1           
        return  [Food foodChar foodPos]

-- main function ---
main :: IO ()
main = do
    level <- generateLevel
    [seekerPos] <- takeRandomPositions 1
    let gameState = Playing { seeker = seekerPos, message = "", over = False }
    initScreen level gameState
    userInput <- getContents
    forM_ (transitions $ playGame level userInput gameState) updateScreen
    putStrLn ""
    
    