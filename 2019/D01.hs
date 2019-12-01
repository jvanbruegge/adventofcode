module D01 where

import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)

readModules :: String -> Maybe [Int]
readModules = traverse readMaybe . lines

solve :: String -> Maybe Int
solve = pure . sum . fmap fuel <=< readModules

solve2 :: String -> Maybe Int
solve2 = pure . sum . fmap (sum . takeWhile (>0) . iterate fuel . fuel) <=< readModules

main :: IO ()
main = do
    input <- readFile "D01.txt"

    putStr "Part 1: "
    print $ fromJust $ solve input

    putStr "Part 2: "
    print $ fromJust $ solve2 input
