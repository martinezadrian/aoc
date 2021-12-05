{--
 - Copyright (c) 2021 H-E-B
 -
 - This software is released under the MIT License.
 - https://opensource.org/licenses/MIT
-}

-- The first parameter to check is the power consumption.

-- You need to use the binary numbers in the diagnostic report to generate two new
-- binary numbers (called the gamma rate and the epsilon rate). The power consumption
-- can then be found by multiplying the gamma rate by the epsilon rate.

import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.List

main :: IO ()
main = day3

-- naive approach
day3 :: IO ()
day3 = interact (show . runDiagnosticB . lines)

runDiagnosticA  :: [String] -> Int
runDiagnosticA diagnostics = do
  let grouped = group . sort <$> transpose diagnostics
      gammaAlgo = maximumBy
      epsilonAlgo = minimumBy
      calculate algo = toDec $ head . algo (comparing length) <$> grouped
  calculate gammaAlgo * calculate epsilonAlgo

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

runDiagnosticB :: [String] -> String
runDiagnosticB diagnostics = do
  show $ oxygenGenerator * co2Scrubber
  where
    oxygenGenerator = toDec $ calculate diagnostics 0 maximumBy
    co2Scrubber = toDec $ calculate diagnostics 0 minimumBy

    calculate [] _ _ = ""
    calculate [x] _ _ = x
    calculate diagnostics pos algo = do
      let grouped = group . sort <$> transpose diagnostics
          newGamma = head ((algo (comparing length) <$> grouped) !! pos)
          newList = filter (\s -> (s !! pos) == newGamma) diagnostics
      calculate newList (pos+1) algo