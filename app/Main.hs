module Main (main) where
import Text.Read (readMaybe)
import qualified Control.Monad.IO.Class
import GameBoard
import RandomIndexGen

main :: IO BoardState
main = do
    let n = 5
    -- | Defines the state of the board. Ensure that boardWidth = boardHeight and boardWidth * boardHeight < bombAmount
    let boardWidth = n
    let boardHeight = n
    let bombAmount = 4
    let possible = [Location x y | x <- [0..boardWidth - 1], y <- [0..boardHeight - 1]]
    indices <- randomIndex boardWidth boardHeight bombAmount
    let locations = map (\i -> possible !! i) indices
    let initGameState = createInitialGameState (createCompleteBoard boardWidth boardHeight locations) boardWidth boardHeight locations
    playGame initGameState

createInitialGameState :: Board -> Int -> Int -> [Location] -> IO BoardState
createInitialGameState g w l bombs = do
    putStrLn ("Creating board with a width of " ++ show w ++ " and a length of " ++ show l ++ ". There are " ++ show (length bombs) ++ " on the board.")
    return BoardState { gameBoard = g, width = w, len = l, bombLocations = bombs, visitedBomb = False, avaliableNoneBombSpaces = w * l - length bombs, visitedSpaces = 0 }

---------------------------
-- Check Win/Lose Condition
---------------------------

isFinalState :: BoardState -> Bool
isFinalState (BoardState _ _ _ _ lose nonBombs turns) = lose || (nonBombs == turns)

-- | True if player won, false if player hit bomb
winOrLose :: BoardState -> Bool
winOrLose (BoardState _ _ _ _ lose _ _) = not lose

-- | Reveals the full board and notifies the user of their victory
winGame :: BoardState -> IO BoardState
winGame bs = do
    putStrLn "You Win!"
    putStrLn (displayBoard bs dispFinishedBoardHelper)
    return bs
-- | Reveals the full board and notifies the user of the defeat
lostGame :: BoardState -> IO BoardState
lostGame bs = do
    putStrLn "You lose!"
    putStrLn (displayBoard bs dispFinishedBoardHelper)
    return bs

-- | Game loop. At each iteration checks if a lose/win condition is met. If not displays current state and starts need turn
playGame :: IO BoardState -> IO BoardState
playGame s = do
    state <- s
    if isFinalState state then (if winOrLose state then winGame else lostGame) state
    else
      do
        putStrLn (displayBoard state dispGameBoardHelper)
        playGame (gameIteration state)

-- | At each iteration, it will ask for a location and return a new state with that has the selected square revealed
gameIteration :: BoardState -> IO BoardState
gameIteration boardState = do
    input <- getUserInput boardState
    let newState = revealLocation input boardState
    return newState

-- | Converts the user input into numbers.
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
getUserInput (BoardState board wid leng w x y i) = do
    putStrLn "Please enter in the form: x y"
    input <- getLine
    case parseInput input of
        Just (a, b) -> do
            if a >= wid || b >= leng || a < 0 || b < 0 || z == Visited
            then getUserInput (BoardState board wid leng w x y i)
            else return (Location a b)
            where Square _ _ _ z = getSquare board (Location a b)
        Nothing     -> getUserInput (BoardState board wid leng w x y i)