module Main where

import AoC                  (Parser, applyInput1, blankP, blanksP, decimal, lexeme)
import Data.List            (transpose)
import Data.Maybe           (catMaybes, isNothing)
import Text.Megaparsec      ((<|>), many, sepEndBy1, some, try)
import Text.Megaparsec.Char (char, digitChar, newline)


type Op = [Int] -> Int


splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p l = case break p l of
    ([], [])  -> []
    (s,  [])  -> [s]
    (s, rest) -> s : splitOn p (drop 1 rest)


solve :: [(Op, [Int])] -> Int
solve = sum . map (uncurry ($))


opP :: Parser Op
opP = (char '+' >> pure sum) <|> (char '*' >> pure product)


cephalopodOrderP :: Parser [(Op, [Int])]
cephalopodOrderP = do
    nums <- try lineP `sepEndBy1` newline
    ops  <- many blanksP >> some (lexeme opP)
    return $ zip ops (toNumLists (transpose nums))
  where
    lineP = some $ (Just <$> digitChar) <|> (char ' ' >> pure Nothing)

    toNumLists =  map catMaybes . splitOn isNothing . map toNum

    toNum maybeNum = case catMaybes maybeNum of
        []   -> Nothing
        num  -> Just $ read num


normalOrderP :: Parser [(Op, [Int])]
normalOrderP = do
    nums <- try numLineP `sepEndBy1` newline
    ops  <- many blanksP >> some (lexeme opP)
    return (zip ops (transpose nums))
  where
    numLineP = many blankP >> some (lexeme decimal)


main :: IO ()
main = do
    applyInput1 normalOrderP solve
    applyInput1 cephalopodOrderP solve
