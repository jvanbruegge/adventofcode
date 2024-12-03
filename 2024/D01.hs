module D01 where

import Data.List (sort)

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.).(.)

readInput :: String -> ([Int], [Int])
readInput = unzip . fmap (toTuple . fmap read . words) . lines
  where toTuple [x, y] = (x, y)

solution1 :: ([Int], [Int]) -> Int
solution1 = sum . uncurry (zipWith (abs ... subtract)) . both sort
  where both f (a, b) = (f a, f b)

solution2 :: ([Int], [Int]) -> Int
solution2 (xs, ys) = sum $ fmap (\x -> x * length (filter (== x) ys)) xs

main :: IO ()
main = do
  input <- readInput <$> readFile "2024/D01.txt"
  print $ solution1 input
  print $ solution2 input
