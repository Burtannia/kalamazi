module Import.Util where

fours :: [a] -> [[a]]
fours xs
    | length xs < 4 = [xs]
    | otherwise = take 4 xs : fours (drop 4 xs)