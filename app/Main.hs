{-# LANGUAGE InstanceSigs #-}
module Main (main) where
import Data.List ()
import System.IO (SeekMode (AbsoluteSeek))
import Data.Char ()
import System.Directory ()
import System.Random (randomRIO)
import Data.Array
import Data.Time (TimeLocale(time12Fmt))

-------------------
-- Data Definitions
-------------------

-- A gameboard is made up of n x m Squares
--  * location = Index of the current square on the gameboard
--  * isMined = Has the player visited the square
--  * neighboringMines = Number of mines directly beside the square
--  * playerMarking = The state a player has interacted with the square
--      - Untouched - Player has not visited
--      - LostMine - Player visited and it was a mine
--      - Flagged - Player has not visited, but marked it with a flag
--      - Visited - Player has visited
--
data Square = Square
    { location :: Location
    , isMine :: Bool
    , neighboringMines :: Int
    , playerMarking :: PlayerMarking
    } deriving (Show)

data Location = Location Int Int deriving (Eq, Show)

data PlayerMarking = Untouched | LostMine | Flagged | Visited deriving (Eq, Show)

-- A gameboard is defined as 
type Board = [[Square]]

-------------------
-- Board Creation
-------------------

createBombLocation :: (Integral a) => Int -> Int -> a -> IO [Location]
createBombLocation len width amt = sequence [ randomLocation | _ <- [1..amt]]
    where
        randomLocation = do
            x <- randomRIO (0, len - 1)
            y <- randomRIO (0, width - 1)
            return (Location x y)


-- Determining the Location around a specific cell
offsets :: [Location]
offsets = [Location x y | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

addOffset :: Location -> Location -> Location
addOffset (Location x1 y1) (Location x2 y2) = Location (x1 + x2) (y1 + y2)

iterateNeighbors :: Location -> Int -> Int -> [Location]
iterateNeighbors selectedLocation width height = [Location x y |
    Location x y <- map (addOffset selectedLocation) offsets,
    x >= 0 && x < width && y >= 0 && y < height
    ]

-- Updating a specific square
-- becased Lists are immutable you'd need to create a new cell every time

incrementBombCount :: Square -> Square
incrementBombCount (Square w x y z) = Square {location = w, isMine = x, neighboringMines = y + 1, playerMarking = z}

updateSquares :: [Location] -> Board -> (Square -> Square) -> Board
updateSquares lst oldBoard f = [[if Location r c `elem` lst then f x else x | (c, x) <- zip [0..] row] | (r, row) <- zip [0..] oldBoard]

-- Creating the overall board
createRow :: Int -> Int -> Int -> [Location] -> [Square]
createRow xPos yPos width bombLocations
    | Location xPos yPos `elem` bombLocations && xPos < width = Square { location = Location xPos yPos, isMine = True , neighboringMines = 0, playerMarking = Untouched } : createRow (xPos + 1) yPos width bombLocations
    | xPos < width = Square { location = Location xPos yPos, isMine = False, neighboringMines = 0, playerMarking = Untouched } : createRow (xPos + 1) yPos width bombLocations
    | otherwise = []

createBoard :: Int -> Int -> Int -> Int -> [Location] -> Board
createBoard xPos yPos width len bombLocations
    | yPos < len = createRow xPos yPos width bombLocations : createBoard xPos (yPos + 1) len width bombLocations
    | otherwise = []

createCompleteBoard :: Int -> Int -> [Location] -> Board
    -- foldr ([Location] -> Board -> Board) -> Initial Board -> [[Location]] -> Final Board
createCompleteBoard width len bombLocations = foldr (\locations accBoard -> updateSquares locations accBoard incrementBombCount) (createBoard 0 0 width len bombLocations) [iterateNeighbors x width len | x <- bombLocations]
-- _addBombNumbers :: Board -> Board
-- _addBombNumbers board = 
-- modifyArray :: Board -> Location -> Square -> Board
-- modifyArray arr pos val = arr // [(pos, val)]

-- Creates a string that is pretty to print.
displayBoard :: Board -> String
displayBoard board = unlines $ map (unwords . map (show . getSquare)) board
    where getSquare (Square (Location x y) isMine neighboringMines _) = " " ++ show neighboringMines ++ " "

main :: IO ()
main = do
    let boardWidth = 8
    let boardHeight = 8
    let bombAmount = 15
    locations <- createBombLocation boardWidth boardHeight bombAmount
    -- locations is no longer an io, so we'd need to pass that into our board generator
    -- print (displayBoard (createBoard 0 0 10 8 locations))
    -- print locations
    let board = createBoard 0 0 boardWidth boardHeight locations
    putStrLn ( displayBoard(board) )
    let newBoard = updateSquares (iterateNeighbors (Location 0 1) boardWidth boardHeight ) board incrementBombCount
    putStrLn ( displayBoard (newBoard))
    putStrLn ( displayBoard(createCompleteBoard boardWidth boardHeight locations))
    -- print ( iterateNeighbors (Location 0 1) boardWidth boardHeight)
    -- print array (((1,1),(3,3)) [((i,j),i*j) | i <- [1..3], j <- [1..3]])
