module D04 where

import Data.List (group)

range :: [Integer]
range = [147981 .. 691423] -- my puzzle input

solve :: [Integer] -> [String]
solve =
  filter (fst . foldl (\(a, x) c -> (a && x <= c, c)) (True, '/'))
    . filter (any (> 1) . fmap length . group)
    . fmap show

solve2 :: [Integer] -> [String]
solve2 =
  filter (elem 2 . fmap length . group)
    . solve

main :: IO ()
main = do
  putStr "Part 1: "
  print $ length $ solve range
  putStr "Part 2: "
  print $ length $ solve2 range
