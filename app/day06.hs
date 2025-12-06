module Main where

import AoC                  (Parser, applyInput1, blankP, blanksP, decimal, lexeme)
import Data.List            (transpose)
import Text.Megaparsec      ((<|>), many, sepEndBy1, some, try)
import Text.Megaparsec.Char (char, digitChar, newline, space)
import Data.Either          (lefts, rights)
import Data.Maybe           (catMaybes, isNothing)


data Op = Sum | Product deriving (Show, Eq)


opToFun :: Op -> [Int] -> Int
opToFun Sum     = sum
opToFun Product = product


splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p l = case break p l of
    ([], [])  -> []
    (s,  [])  -> [s]
    (s, rest) -> s : splitOn p (drop 1 rest)


solve :: [([Int], Op)] -> Int
solve = sum . map (\(nums, op) -> opToFun op nums)


opP :: Parser Op
opP = (char '+' >> pure Sum) <|> (char '*' >> pure Product)


cephalopodOrderP :: Parser [([Int], Op)]
cephalopodOrderP = do
    numLines <- try lineP `sepEndBy1` newline
    ops <- many blanksP >> some (lexeme opP)
    let nums = toNumLists $ transpose numLines
    return $ zip nums ops
  where
    lineP = some $ (Just . read . (:[]) <$> digitChar) <|> (char ' ' >> pure Nothing)

    toNumLists :: [[Maybe Int]] -> [[Int]]
    toNumLists =  map catMaybes . splitOn isNothing . map toNum

    toNum :: [Maybe Int] -> Maybe Int
    toNum maybeNum = case catMaybes maybeNum of
        []   -> Nothing
        num  -> Just $ foldl' (\acc x -> acc * 10 + x) 0 num


normalOrderP :: Parser [([Int], Op)]
normalOrderP = do
    lns <- lineP `sepEndBy1` space
    let nums = lefts lns
        ops  = concat $ rights lns
    return (zip (transpose nums) ops)
  where
    lineP = many blankP >> (Left <$> some (lexeme decimal)) <|> (Right <$> some (lexeme opP))


main :: IO ()
main = do
    applyInput1 normalOrderP solve
    applyInput1 cephalopodOrderP solve
