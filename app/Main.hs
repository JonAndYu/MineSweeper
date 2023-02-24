{-# LANGUAGE InstanceSigs #-}
module Main (main) where
import Data.List ()
import System.IO ()
import Data.Char ()
import System.Directory ()
import System.Random (randomRIO)

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

data PlayerMarking = Untouched | LostMine | Flagged | Visited deriving (Show)

-- A gameboard is defined as 
type Board = [[Square]]

-------------------
-- Board Creation
-------------------

createBombLocation :: (Integral a) => Int -> Int -> a -> IO [Location]
createBombLocation len width amt = sequence [ randomLocation | _ <- [1..amt]]
    where
        randomLocation = do
            x <- randomRIO (0, len)
            y <- randomRIO (0, width)
            return (Location x y)

createRow :: Int -> Int -> Int -> [Location] -> [Square]
createRow xPos yPos width bombLocations
    | Location xPos yPos `elem` bombLocations = Square { location = Location xPos yPos, isMine = True , neighboringMines = 0, playerMarking = Untouched } : createRow (xPos + 1) yPos width bombLocations
    | xPos < width = Square { location = Location xPos yPos, isMine = False, neighboringMines = 0, playerMarking = Untouched } : createRow (xPos + 1) yPos width bombLocations
    | otherwise = []

createBoard :: Int -> Int -> Int -> Int -> [Location] -> Board
createBoard xPos yPos width len bombLocations 
    | yPos < len = createRow xPos yPos width bombLocations : createBoard xPos (yPos + 1) len width bombLocations
    | otherwise = []

-- Creates a string that is pretty to print.
displayBoard :: Board -> String
displayBoard board = unlines $ map (unwords . map (show . getSquare)) board
    where getSquare (Square (Location x y) _ _ _) = "( " ++ show x ++ "," ++ show y ++ " ) "


main :: IO ()
main = do

    locations <- createBombLocation 10 8 10
    -- locations is no longer an io, so we'd need to pass that into our board generator
    -- print (displayBoard (createBoard 0 0 10 8 locations))
    -- print locations
    putStrLn ( displayBoard (createBoard 0 0 10 10 locations))
