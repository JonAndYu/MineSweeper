module RandomIndexGen ( randomIndex ) where

import System.Random (randomRs, newStdGen)
import Data.List (nub)
import qualified Control.Monad.IO.Class

{- | Creates a list of random Locations which comprises of (x, y) coordinates, 0 <= x < len and 0 <= y < width
TODO: https://github.com/JonAndYu/MineSweeper/issues/3
TLDR: Potentially generates the same location, especially for small boards, We need them to be n unique locations.
-}
randomIndex :: Control.Monad.IO.Class.MonadIO m => Int -> Int -> Int -> m [Int]
randomIndex len width amt = do
    g <- newStdGen
    return . take amt . nub $ (randomRs (0, len * width - 1) g :: [Int])