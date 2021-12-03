module Lib where

import Text.Read (readMaybe)

readInt :: String -> Maybe Int
readInt = readMaybe
