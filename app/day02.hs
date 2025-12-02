module Main where

import           AoC                        (applyInput, decimal, groupsOf, lexeme, symbol)
import           Text.Megaparsec            (sepEndBy1)
import           Text.Megaparsec.Char       (space)
import qualified Text.Megaparsec.Char.Lexer as L


toDigitList :: Int -> [Int]
toDigitList = toDigitListAux []
  where
    toDigitListAux acc x
        | x == 0    = acc
        | otherwise = toDigitListAux (x `mod` 10 : acc) (x `div` 10)


isInvalidP2 :: [Int] -> Bool
isInvalidP2 digits =
    any groupsOfValid [1 .. len `div` 2]
  where
    len = length digits

    groupsOfValid n
        | len `mod` n == 0 = allEqual $ groupsOf n digits
        | otherwise        = False

    allEqual []     = True
    allEqual (x:xs) = all (==x) xs


isInvalidP1 :: [Int] -> Bool
isInvalidP1 digits = take halfLen digits == drop halfLen digits
  where
    halfLen = length digits `div` 2


rangeInvalidIds :: ([Int] -> Bool) -> (Int, Int) -> [Int]
rangeInvalidIds isInvalid (start, end) = filter (isInvalid . toDigitList) [start .. end]


solve :: ([Int] -> Bool) -> [(Int, Int)] -> Int
solve isInvalid = sum . concatMap (rangeInvalidIds isInvalid)


main :: IO ()
main = applyInput intervalsP (solve isInvalidP1) (solve isInvalidP2)
  where
    intervalsP = lexeme intervalP `sepEndBy1` L.symbol space ","

    intervalP = do
        start <- lexeme decimal <* symbol "-"
        end   <- lexeme decimal
        return (start, end)