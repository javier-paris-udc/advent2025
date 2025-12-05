module Main where

import AoC                  (Parser, applyInput, decimal, symbol)
import Data.List            (sort)
import Data.Vector          (Vector, (!), fromList, toList)
import Text.Megaparsec      (sepEndBy1)
import Text.Megaparsec.Char (newline, space)


intervalLength :: (Int, Int) -> Int
intervalLength (st, end) = end - st + 1


fuseIntervals :: [(Int, Int)] -> [(Int, Int)]
fuseIntervals l = case l of
    (st1, end1) : (st2, end2) : ivals ->
        if st2 <= end1
            then fuseIntervals ((st1, max end1 end2) : ivals)
            else (st1, end1) : fuseIntervals ((st2, end2) : ivals)
    ivals -> ivals


binSearch :: Vector (Int, Int) -> Int -> Bool
binSearch intervals n = search 0 (length intervals - 1)
  where
    search st end
        | st > end  = False
        | otherwise =
            let mid = (st + end) `div` 2
                (iStart, iEnd) = intervals ! mid
            in iStart <= n && n <= iEnd ||
                if n < iStart then search st (mid - 1) else search (mid + 1) end


solveP2 :: (Vector (Int, Int), [Int]) -> Int
solveP2 (intervals, _) = sum $ map intervalLength $ fuseIntervals $ toList intervals


solveP1 :: (Vector (Int, Int), [Int]) -> Int
solveP1 (intervals, nums) = length $ filter (binSearch intervals) nums


ingredientsP :: Parser (Vector (Int, Int), [Int])
ingredientsP = liftA2 (,) (fmap fromList intervalsP <* space) (decimal `sepEndBy1` newline)
  where
    intervalsP = fmap sort (liftA2 (,) (decimal <* symbol "-") decimal `sepEndBy1` newline)


main :: IO ()
main = applyInput ingredientsP solveP1 solveP2