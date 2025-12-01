module Main where


import AoC                  (applyInput, intP)
import Data.List            (scanl')
import Text.Megaparsec      ((<|>), sepEndBy1)
import Text.Megaparsec.Char (char, space1)


data Rotation = L | R deriving (Show, Eq)


rotationToSign :: Rotation -> Int
rotationToSign s =
    case s of
        L -> 1
        R -> -1


rotate :: Int -> (Rotation, Int) -> Int
rotate pos (dir, cnt) =
    (pos + cnt * rotationToSign dir) `mod` 100


solveP2 :: [(Rotation, Int)] -> Int
solveP2 = sum . countCrosses . scanl' rotateP2 50
  where
    rotateP2 pos (dir, cnt) =
        pos + cnt * rotationToSign dir

    countCrosses poss = zipWith countCross poss (drop 1 poss)

    countCross start end
        | end > start && end `mod` 100 < start `mod` 100 =
            (end - start) `div` 100 + 1
        | end > start =
            (end - start) `div` 100
        | start `mod` 100 == 0 =
            (start - end) `div` 100
        | end `mod` 100 == 0 || end `mod` 100 > start `mod` 100 =
            (start - end) `div` 100 + 1
        | otherwise =
            (start - end) `div` 100


solveP1 :: [(Rotation, Int)] -> Int
solveP1 = length . filter (== 0) . scanl' rotate 50


main :: IO ()
main = applyInput (rotationP `sepEndBy1` space1) solveP1 solveP2
  where
    rotationP = do
        dir <- (char 'L' >> pure L) <|> (char 'R' >> pure R)
        rots <- intP
        return (dir, rots)