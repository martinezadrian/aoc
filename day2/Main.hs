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
main = day2B

-- naive approach
day2 :: IO ()
day2 = interact (show . pilotSubmarine . mapMaybe readCommand . lines)

readCommand :: String -> Maybe Command
readCommand = readMaybe

pilotSubmarine :: [Command] -> Position
pilotSubmarine = foldl runCommand startingPosition

startingPosition :: Position
startingPosition = Position 0 0

runCommand :: Position -> Command -> Position
runCommand (Position h d) command =
  case command of
    Forward x -> Position (h + x) d
    Down y -> Position h (d + y)
    Up z -> Position h (d - z)

data Command =
    Forward Horizontal
  | Down Depth
  | Up Depth
  deriving (Read)

data Position = Position Horizontal Depth

instance Show Position where
  show (Position h d) = show (h * d)

type Horizontal = Int
type Depth = Int
type Aim = Int

data PositionB = PositionB Horizontal Depth Aim

instance Show PositionB where
  show (PositionB h d a) = show (h * d)

day2B :: IO ()
day2B = interact (show . pilotSubmarineB . mapMaybe readCommand . lines)

pilotSubmarineB :: [Command] -> PositionB
pilotSubmarineB = foldl runCommandB startingPositionB

startingPositionB :: PositionB
startingPositionB = PositionB 0 0 0

runCommandB :: PositionB -> Command -> PositionB
runCommandB (PositionB h d a) command =
  case command of
    Forward x -> PositionB (h + x) (d + (a * x)) a
    Down y -> PositionB h d (a + y)
    Up z -> PositionB h d (a - z)
