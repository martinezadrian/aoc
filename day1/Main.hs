{--
 - Copyright (c) 2021 H-E-B
 -
 - This software is released under the MIT License.
 - https://opensource.org/licenses/MIT
-}

module Main where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = day1b

-- naive approach
day1 :: IO ()
day1 = interact (show . sonarCheck . mapMaybe readInt . lines)

readInt :: String -> Maybe Int
readInt = readMaybe

sonarCheck :: [Int] -> Int
sonarCheck (x:y:xs) = (if x < y then 1 else 0) + sonarCheck (y : xs)
sonarCheck [x] = 0
sonarCheck [] = 0

day1b :: IO ()
day1b = interact (show . sonarCheckB . mapMaybe readInt . lines)

sonarCheckB :: [Int] -> Int
sonarCheckB (x:y:z:f:xs) =
  (if (x + y + z) < (y + z + f) then 1 else 0) + sonarCheckB (y : z : f : xs)
sonarCheckB [] = 0
sonarCheckB [x] = 0
sonarCheckB [x,y] = 0
sonarCheckB [x,y,z] = 0
