module D01 where

import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

calcModuleMass :: Int -> Int
calcModuleMass = subtract 2 . (`div` 3)

readModules :: String -> Maybe [Int]
readModules = traverse readMaybe . lines

solve :: String -> Maybe Int
solve = pure . sum . fmap calcModuleMass <=< readModules

solve2 :: String -> Maybe Int
solve2 = pure . sum . fmap (calcAdditionalFuel 0 . calcModuleMass) <=< readModules
    where calcAdditionalFuel :: Int -> Int -> Int
          calcAdditionalFuel n x = let fuel = x `div` 3 - 2
              in if fuel <= 0
                 then n + x
                 else calcAdditionalFuel (n + x) fuel

main :: IO ()
main = do
    input <- readFile "D01.txt"

    putStr "Part 1: "
    print $ fromJust $ solve input

    putStr "Part 2: "
    print $ fromJust $ solve2 input
