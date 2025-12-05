module Main where

import AoC (applyInput)
import Text.Megaparsec ((<|>), sepEndBy1, some)
import Text.Megaparsec.Char (char, space)
import qualified Data.HashSet as S
import Data.HashSet (HashSet)


isAccessible :: HashSet (Int, Int) -> (Int, Int) -> Bool
isAccessible grid (x, y) =
    let neighs = filter (/= (x,y)) $ liftA2 (,) [x - 1 .. x + 1] [y - 1 .. y + 1]
        rolls  = length $ filter (`S.member` grid) neighs
    in rolls < 4



solveP2 :: HashSet (Int, Int) -> Int
solveP2 grid0 = S.size grid0 - S.size (removeAll grid0)
  where
    removeAll grid =
        let newGrid = S.filter (not . isAccessible grid) grid
        in if grid == newGrid then grid
           else removeAll newGrid


solveP1 :: HashSet (Int, Int) -> Int
solveP1 grid = length $ filter (isAccessible grid) $ S.toList grid


main :: IO ()
main = applyInput gridP solveP1 solveP2
  where
    gridP = do
        rows <-  some rollP `sepEndBy1` space
        let nRows = length rows
            nCols = case rows of
                []  -> error "empty file"
                r:_ -> length r
            coords = liftA2 (,) [0 .. nRows - 1] [0 .. nCols - 1]
            rolls  = map fst $ filter snd $ zip coords (concat rows)
        return $ S.fromList rolls

    rollP = (char '.' >> pure False) <|> (char '@' >> pure True)