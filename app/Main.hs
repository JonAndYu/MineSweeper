{-# LANGUAGE InstanceSigs #-}
module Main (main) where
import Data.List (nub)
import Data.Char ()
import System.Directory ()
import System.Random (randomRs, newStdGen)
import Text.Read (readMaybe)

-------------------
-- Data Definitions
-------------------

{- | A gameboard is made up of n x m Squares
    location = Index of the current square on the gameboard
    isMined = Has the player visited the square
    neighboringMines = Number of mines directly beside the square
    playerMarking = The state a player has interacted with the square
        Untouched - Player has not visited
        LostMine - Player visited and it was a mine
        Flagged - Player has not visited, but marked it with a flag
        Visited - Player has visited
-}
data Square = Square
    { location :: Location
    , isMine :: Bool
    , neighboringMines :: Int
    , playerMarking :: PlayerMarking
    } deriving (Show)

data Location = Location Int Int deriving (Eq, Show)

data PlayerMarking = Untouched | LostMine | Flagged | Visited deriving (Eq, Show)

data BoardState = BoardState
    { gameBoard :: Board
    , width :: Int
    , len :: Int
    , bombLocations :: [Location]
    , visitedBomb :: Bool
    , avaliableNoneBombSpaces :: Int
    , visitedSpaces :: Int
    }

-- A gameboard is defined as 
type Board = [[Square]]

-------------------------
-- Board Creation Helpers
-------------------------

help :: (Int, Int) -> Location
help (x, y) = Location x y

{- | Creates a list of random Locations which comprises of (x, y) coordinates, 0 <= x < len and 0 <= y < width
TODO: https://github.com/JonAndYu/MineSweeper/issues/3
TLDR: Potentially generates the same location, especially for small boards, We need them to be n unique locations.
-}
randomIndex len width amt = do
    g <- newStdGen
    return . take amt . nub $ (randomRs (0, len * width - 1) g :: [Int])

-- | A mask of locations of all cardinal + ordinal directions from a given point, not including itself.
offsets :: [Location]
offsets = [Location x y | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

addOffset :: Location -> Location -> Location
addOffset (Location x1 y1) (Location x2 y2) = Location (x1 + x2) (y1 + y2)

{- | Returns a list of locations around a given point using the offsets mask and addOffset function.
Includes boundary checks to ensure that the neighbors are within the area of the Board. 
-}
iterateNeighbors :: Location -> Int -> Int -> [Location]
iterateNeighbors selectedLocation width height = [Location x y |
    Location x y <- map (addOffset selectedLocation) offsets,
    x >= 0 && x < width && y >= 0 && y < height
    ]

-------------------
-- Board Updates
-------------------

-- | Helper function to access a specfic square on a board
getSquare :: Board -> Location -> Square
getSquare board (Location x y) = board !! y !! x

-- | Helper function to modify a squares PlayerMarking
revealMarking :: Square -> Square
revealMarking (Square w x y z) = Square {location = w, isMine = x, neighboringMines = y, playerMarking = if x then LostMine else Visited }

-- | Helper function to increment the bomb count at a given square.
incrementBombCount :: Square -> Square
incrementBombCount (Square w x y z) = Square {location = w, isMine = x, neighboringMines = (if x then 0 else 1) + y, playerMarking = z}

revealLocation :: Location -> BoardState -> BoardState
revealLocation location (BoardState g w l locations visitBomb avaNoneBombs visitedCount) = BoardState
    { gameBoard = updateSquares [location] g revealMarking
    , width = w
    , len = l
    , bombLocations = locations
    , visitedBomb = (location `elem` locations) || visitBomb
    , avaliableNoneBombSpaces = avaNoneBombs
    , visitedSpaces = visitedCount + 1
    }


{- | Abstracted function that takes in:
        [Location]          - A list of locations that need to be modified.
        Board               - The current board that needs to be changed.
        (Square -> Square)  - A function that modifies a Square.
    And returns a new board that contains the changes. Because lists are immutable every time this function runs you're creating a new
    Board.
-}
updateSquares :: [Location] -> Board -> (Square -> Square) -> Board
updateSquares lst oldBoard f = [[if Location c r `elem` lst then f x else x | (c, x) <- zip [0..] row] | (r, row) <- zip [0..] oldBoard]

-------------------
-- Board Creation
-------------------

{- | Recursively creates a single row of the board. 
    Populates the row with squares of default values:
        Int         - The current column of the square.
        Int         - The current row of the square. (Never changes)
        Int         - The maximum number of columns in a row.
        [Location]  - A list of locations that contain a bomb.
    By default Squares are unvisited, have 0 neighboringMines, and are placed at the current (xPos, yPos) location.
    This function will check to if the current location is a bombLocation, if so it'll set the square's isMine to be True.
-}
createRow :: Int -> Int -> Int -> [Location] -> [Square]
createRow xPos yPos width bombLocations
    | Location xPos yPos `elem` bombLocations && xPos < width =
        Square { location = Location xPos yPos, isMine = True , neighboringMines = 0, playerMarking = Untouched }
        : createRow (xPos + 1) yPos width bombLocations
    | xPos < width = Square { location = Location xPos yPos, isMine = False, neighboringMines = 0, playerMarking = Untouched } : createRow (xPos + 1) yPos width bombLocations
    | otherwise = []

{- | Recursively creates the empty board with the help of createRow.
    Populates the Board with [squares] (rows) of default values:
        Int         - The current column of the square. (Never changes)
        Int         - The current row of the square.
        Int         - The maximum number of rows in the board.
        [Location]  - A list of locations that contain a bomb.
-}
createEmptyBoard :: Int -> Int -> Int -> Int -> [Location] -> Board
createEmptyBoard xPos yPos width len bombLocations
    | yPos < len = createRow xPos yPos width bombLocations : createEmptyBoard xPos (yPos + 1) len width bombLocations
    | otherwise = []

-- | Given an empty board returned by createEmptyBoard, this function will increment every non bomb square by the number of bombs surrounding it.
-- | Pseudo Type: foldr ([Location] -> Board -> Board) -> Initial Board -> [[Location]] -> Final Board  
createCompleteBoard :: Int -> Int -> [Location] -> Board
createCompleteBoard width len bombLocations = foldr (\locations accBoard -> updateSquares locations accBoard incrementBombCount) (createEmptyBoard 0 0 width len bombLocations) [iterateNeighbors x width len | x <- bombLocations]

createInitialGameState :: Board -> Int -> Int -> [Location] ->IO BoardState
createInitialGameState g w l bombs = do
    putStrLn ("Creating board with a width of " ++ show w ++ " and a length of " ++ show l ++ ". There are " ++ show (length bombs) ++ " on the board.")
    return BoardState { gameBoard = g, width = w, len = l, bombLocations = bombs, visitedBomb = False, avaliableNoneBombSpaces = w * l - length bombs, visitedSpaces = 0 }

createFinishedGame :: BoardState -> IO BoardState
createFinishedGame _ = do
    putStrLn "Rerun the program to try again."
    return BoardState { gameBoard = [[]], width = 0, len = 0, bombLocations = [], visitedBomb = False, avaliableNoneBombSpaces = 0, visitedSpaces = 0}
-- Creates a string that is pretty to print.
displayBoard :: BoardState -> String
displayBoard (BoardState g _ _ _ _ _ _) = unlines $ map (unwords . map (show . getSquare)) g
    -- where getSquare (Square (Location x y) isMine neighboringMines _) = "(" ++ (if isMine then "M:" else "o:") ++ show neighboringMines ++ ")"
    -- where getSquare (Square (Location x y) isMine neighboringMines _) = "("++ show x ++ "," ++ show y++")"
  where
    getSquare (Square (Location _ _) _ neighboringMines playerMarking)
        | playerMarking == Untouched = " - "
        | playerMarking == Visited = " " ++ show neighboringMines ++ " "
        | playerMarking == Flagged = " F "
        | otherwise = " X " -- Bomb

displayFinishedBoard :: BoardState ->  String
displayFinishedBoard (BoardState g _ _ _ _ _ _) = unlines $ map (unwords . map (show . getSquares)) g
    where getSquares (Square _ isMine neighboringMines _) = " " ++ (if isMine then "X" else show neighboringMines ) ++ " "
    -- where getSquare (Square (Location x y) isMine neighboringMines _) = "("++ show x ++ "," ++ show y++")"

parseInput :: String -> Maybe (Int, Int)
parseInput input = case words input of
                     [a, b] -> case (readMaybe a, readMaybe b) of
                                 (Just x, Just y) -> Just (x, y)
                                 _                -> Nothing
                     _      -> Nothing

{- | Given the length and width of a game board, 
getUserInput obtains a valid location on the board.
A location is valid if:
    - It is unvisited
    - 0 <= x < width
    - 0 <= y < length
-}
getUserInput :: BoardState -> IO Location
getUserInput (BoardState board width len w x y i) = do
    putStrLn "Please enter in the form: x y"
    input <- getLine
    case parseInput input of
        Just (a, b) -> do
            if z == Visited
            then getUserInput (BoardState board width len w x y i)
            else return (Location a b)
            where Square _ _ _ z = getSquare board (Location a b)
        Nothing     -> getUserInput (BoardState board width len w x y i)

gameLoop :: BoardState -> IO BoardState
gameLoop boardState = do
    input <- getUserInput boardState
    let newState = revealLocation input boardState
    return newState

main :: IO (IO BoardState)
main = do
    let boardWidth = 8
    let boardHeight = 8
    let bombAmount = 15
    let possible = [Location x y | x <- [0..boardWidth - 1], y <- [0..boardHeight - 1]]
    indices <- randomIndex boardWidth boardHeight bombAmount
    let locations = map (\i -> possible !! i) indices
    game (createInitialGameState (createCompleteBoard boardWidth boardHeight locations) boardWidth boardHeight locations)

isFinalState :: BoardState -> Bool
isFinalState (BoardState _ _ _ _ lose nonBombs turns) = lose || (nonBombs == turns)

-- | True if player won, false if player hit bomb
winOrLose :: BoardState -> Bool
winOrLose (BoardState _ _ _ _ lose _ _) = not lose

winGame :: BoardState -> IO (IO BoardState)
winGame bs = do
    putStrLn "You Win!"
    putStrLn (displayFinishedBoard bs)
    return (createFinishedGame bs)

lostGame :: BoardState -> IO (IO BoardState)
lostGame bs = do
    putStrLn "You lose!"
    putStrLn (displayFinishedBoard bs)
    return (createFinishedGame bs)

game :: IO BoardState -> IO (IO BoardState)
game oldState = do
    state <- oldState
    if isFinalState state then (if winOrLose state then winGame else lostGame) state
    else
      do
        putStrLn (displayBoard state)
        game (gameLoop state)
