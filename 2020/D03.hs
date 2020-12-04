{-# LANGUAGE BangPatterns #-}

module D03 where

import Data.Foldable (foldl')

takeNth :: Int -> [a] -> [a]
takeNth n = fmap snd . filter ((== 0) . (`mod` n) . fst) . zip [0 ..]

solve :: (Int, Int) -> String -> Int
solve (x, y) = snd . foldl' go (x, 0) . tail . takeNth y . lines
  where
    go (!xCoord, !n) s = (xCoord + x, if idx xCoord s == '.' then n else n + 1)
    idx x s = s !! (x `mod` length s)

main :: IO ()
main = do
  input <- readFile "D03.txt"
  putStr "Part 1: "
  print $ solve (3, 1) input
  putStr "Part 2: "
  print $ product $ solve <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] <*> [input]
