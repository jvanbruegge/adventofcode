module D02 where

import Data.Foldable (foldl')

readInput :: String -> [[Int]]
readInput = fmap (fmap read . words) . lines

count :: (a -> Bool) -> [a] -> Int
count f = foldl' (\n a -> if f a then n + 1 else n) 0

safe :: [Int] -> Bool
safe xs = (linear (<) xs || linear (>) xs) && levels xs
  where
    levels (x:xs) = and $ zipWith (\a b -> let x = abs (a - b) in 1 <= x && x <= 3) (x:xs) xs
    linear f (x:xs) = and $ zipWith f (x:xs) xs

solution1 :: [[Int]] -> Int
solution1 = count safe

solution2 :: [[Int]] -> Int
solution2 = count (any safe . drop1s)
  where
    drop1s [] = []
    drop1s (x:xs) = xs : fmap (x:) (drop1s xs)


main :: IO ()
main = do
  input <- readInput <$> readFile "2024/D02.txt"
  print $ solution1 input
  print $ solution2 input
