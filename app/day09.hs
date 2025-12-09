module Main where

import AoC (applyInput1, decimal)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec (sepEndBy1)


area :: ((Int, Int), (Int, Int)) -> Int
area ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)


solveP1 :: [(Int, Int)] -> Int
solveP1 redTiles = maximum $ map area pairs
  where
    pairs = [(p1, p2) | p1 <- redTiles, p2 <- redTiles, p1 < p2]



main :: IO ()
main = applyInput1 (liftA2 (,) (decimal <* char ',') decimal `sepEndBy1` space) solveP1