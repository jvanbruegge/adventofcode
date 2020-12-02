module D01 where

import Control.Monad ((<=<), replicateM)
import Data.Maybe (fromJust, listToMaybe)
import Text.Read (readMaybe)

solve :: Int -> String -> Maybe Int
solve n = listToMaybe . fmap product . filter ((==) 2020 . sum) . replicateM n <=< traverse readMaybe . lines

main :: IO ()
main = do
  input <- readFile "D01.txt"
  putStr "Part 1: "
  print $ fromJust $ solve 2 input
  putStr "Part 2: "
  print $ fromJust $ solve 3 input
