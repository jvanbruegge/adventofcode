module D03 where

import Text.Regex.TDFA ((=~))

mulRegex :: String
mulRegex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

calc :: [[String]] -> Int
calc = sum . fmap (product . fmap read)

solution1 :: String -> Int
solution1 s = calc $ fmap (drop 1) (s =~ mulRegex)

solution2 :: String -> Int
solution2 s = calc $ filterCmds True $ s =~ (mulRegex <> "|don't\\(\\)|do\\(\\)")
  where
    filterCmds :: Bool -> [[String]] -> [[String]]
    filterCmds _ [] = []
    filterCmds _ (("don't()":_):xs) = filterCmds False xs
    filterCmds _ (("do()":_):xs) = filterCmds True xs
    filterCmds True (x:xs) = drop 1 x : filterCmds True xs
    filterCmds False (_:xs) = filterCmds False xs

main :: IO ()
main = do
  input <- readFile "2024/D03.txt"
  print $ solution1 input
  print $ solution2 input
