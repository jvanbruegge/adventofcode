{-# LANGUAGE OverloadedStrings #-}

module D04 where

import Data.Char (isDigit)
import qualified Data.Text as T
import Text.Read (readMaybe)

solve :: (T.Text -> Bool) -> T.Text -> Int
solve validate = length . filter (== 7) . fmap (length . filter (not . ("cid" `T.isPrefixOf`)) . filter validate . T.words) . T.splitOn "\n\n"

validateFields :: T.Text -> Bool
validateFields = go . T.splitOn ":"
  where
    go ["byr", x] = between x 1920 2002
    go ["iyr", x] = between x 2010 2020
    go ["eyr", x] = between x 2020 2030
    go ["hgt", x] | Just h <- T.stripSuffix "cm" x, Just n <- readText h = n >= 150 && n <= 193
    go ["hgt", x] | Just h <- T.stripSuffix "in" x, Just n <- readText h = n >= 59 && n <= 76
    go ["hcl", x] | Just t <- T.stripPrefix "#" x = T.length x == 7 && T.all (\c -> isDigit c || (c >= 'a' && c <= 'f')) t
    go ["ecl", x] = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    go ["pid", x] = T.length x == 9 && T.all isDigit x
    go ["cid", _] = True
    go _ = False
    between x min max | Just n <- readText x = T.length x == 4 && n >= min && n <= max
    between _ _ _ = False
    readText = readMaybe . T.unpack

main :: IO ()
main = do
  input <- T.pack <$> readFile "D04.txt"
  putStr "Part 1: "
  print $ solve (const True) input
  putStr "Part 2: "
  print $ solve validateFields input
