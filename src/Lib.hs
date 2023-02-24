module Lib
    ( someFunc
    ) where

import Data.List
import System.IO
import System.Random        -- cabal install random
import Data.Char
import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad (when)
import System.Directory

someFunc :: IO ()
someFunc = putStrLn "someFunc"
