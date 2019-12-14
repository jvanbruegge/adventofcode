{-# LANGUAGE OverloadedStrings #-}

module D03 where

import Data.Bifunctor (bimap)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

data Direction = D | U | L | R

parseInput :: Text -> Maybe ([Direction], [Direction])
parseInput s = do
  [w1, w2] <-
    sequence $
      fmap concat . traverse (parse . T.unpack) . T.splitOn "," <$> T.lines s
  pure (w1, w2)
  where
    parse (x : xs)
      | Just n <- readMaybe xs = case x of
        'D' -> Just $ replicate n D
        'U' -> Just $ replicate n U
        'L' -> Just $ replicate n L
        'R' -> Just $ replicate n R
        _ -> Nothing
    parse _ = Nothing

createMap :: [Direction] -> Map (Int, Int) Int
createMap =
  fst3
    . foldl'
      (\(m, t, x) d -> let t' = f d t in (M.insert t' x m, t', x + 1))
      (M.empty, (0, 0), 1)
  where
    f D = bimap id dec
    f U = bimap id inc
    f L = bimap dec id
    f R = bimap inc id
    inc = (+ 1)
    dec = subtract 1
    fst3 (a, _, _) = a

solve :: Text -> Maybe Int
solve s = do
  (a, b) <- parseInput s
  pure
    $ minimum
    $ fmap (uncurry (+) . bimap abs abs . fst)
    $ M.toList
    $ M.intersection (createMap a) (createMap b)

solve2 :: Text -> Maybe Int
solve2 s = do
  (a, b) <- parseInput s
  pure
    $ minimum
    $ fmap snd
    $ M.toList
    $ M.intersectionWith (+) (createMap a) (createMap b)

main :: IO ()
main = do
  input <- TIO.readFile "D03.txt"
  putStr "Part 1: "
  print $ fromJust $ solve input
  putStr "Part 2: "
  print $ fromJust $ solve2 input
