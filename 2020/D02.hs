module D02 where

import Control.Monad ((<=<))
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~~), getAllTextMatches)

solve :: ((Int, Int, Char, String) -> Bool) -> String -> Maybe Int
solve check = fmap (length . filter (== True)) . traverse (fmap check . parse <=< fmap getAllTextMatches . (=~~ "[0-9]+|[a-z]+")) . lines
  where
    parse [a, b, [x], p]
      | Just l <- readMaybe a,
        Just h <- readMaybe b =
        Just (l, h, x, p)
    parse _ = Nothing

check1 :: (Int, Int, Char, String) -> Bool
check1 (l, h, x, p) = n >= l && n <= h
  where
    n = length $ filter (== x) p

check2 :: (Int, Int, Char, String) -> Bool
check2 (l, h, x, p) = test l /= test h
  where
    test i = p !! (i - 1) == x

main :: IO ()
main = do
  input <- readFile "D02.txt"
  putStr "Part 1: "
  print $ solve check1 input
  putStr "Part 2: "
  print $ solve check2 input
