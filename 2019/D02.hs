{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module D02 where

import Control.Monad ((<=<))
import Control.Monad.ST (ST)
import Data.Maybe (fromJust)
import Data.Text (Text, splitOn, unpack)
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Vector ((!), (!?), (//), Vector)
import qualified Data.Vector.Mutable as M
import Text.Read (readMaybe)

readInput :: Text -> Maybe (Vector Int)
readInput = fmap V.fromList . traverse (readMaybe . unpack) . splitOn ","

runProgram :: Vector Int -> Vector Int
runProgram = V.modify (go 0)
  where
    go :: forall s. Int -> M.MVector s Int -> ST s ()
    go i v = do
      let read = M.read v
          write = M.write v
      x <- read i
      if x == 99
        then pure ()
        else do
          a <- read =<< read (i + 1)
          b <- read =<< read (i + 2)
          ix <- read (i + 3)
          if x == 1
            then write ix (a + b) *> go (i + 4) v
            else
              if x == 2
                then write ix (a * b) *> go (i + 4) v
                else pure ()

solve :: Text -> Maybe Int
solve = (!? 0) . runProgram <=< readInput

solve2 :: Text -> Maybe Int
solve2 s = do
  v <- readInput s
  let inputs = [[(1, x), (2, y)] | x <- [0 .. 99], y <- [0 .. 99]]
      vs = fmap ((! 0) . runProgram . (//) v) inputs
      res = fmap fst $ filter ((==) 19690720 . snd) $ zip (fmap (fmap snd) inputs) vs
  [[x, y]] <- pure res
  pure $ 100 * x + y

main :: IO ()
main = do
  input <- T.readFile "D02.txt"
  putStr "Part 1: "
  print $ fromJust $ solve input
  putStr "Part 2: "
  print $ fromJust $ solve2 input
