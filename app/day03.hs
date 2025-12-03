module Main where

import AoC                  (applyInput, digit)
import Text.Megaparsec      (sepEndBy1, some)
import Text.Megaparsec.Char (space)


digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\n x -> n * 10 + x) 0


getMaxAndRest :: Ord a => [a] -> (a, [a])
getMaxAndRest l =
    case l of
        []     -> error "empty list"
        [x]    -> (x, [])
        x : xs ->
            let (rMax, rTail) = getMaxAndRest xs in
                if x >= rMax
                    then (x, xs)
                    else (rMax, rTail)


getHighestJoltage :: Int -> [Int] -> [Int]
getHighestJoltage n bank
        | n == 0       = []
        | bankLen == n = bank
        | otherwise    =
            let firstDigitList = take (bankLen - n + 1) bank
                rest           = drop (bankLen - n + 1) bank
                (d1, dRest)    = getMaxAndRest firstDigitList
                bankRest       = dRest ++ rest
            in d1 : getHighestJoltage (n - 1) bankRest
      where
        bankLen = length bank


solve :: Int -> [[Int]] -> Int
solve n = sum . map (digitsToInt . getHighestJoltage n)


main :: IO ()
main = applyInput (some digit `sepEndBy1` space) (solve 2) (solve 12)