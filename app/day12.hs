module Main where

import AoC                  (Parser, applyInput1, blanksP, decimal, lexeme)
import Text.Megaparsec      ((<|>), sepEndBy1, some, try)
import Text.Megaparsec.Char (char, space)


hasSolution :: (Int, Int, [Int]) -> Bool
hasSolution (x, y, pieces_cnt) =
    maxPieces >= sum pieces_cnt
  where
    maxPieces = xPieces * yPieces
    xPieces = x `div` 3
    yPieces = y `div` 3


noSolution :: [Int] -> (Int, Int, [Int]) -> Bool
noSolution pieces (x, y, pieces_cnt) =
    totalSqs > size
  where
    size = x * y
    totalSqs = sum $ zipWith (*) pieces pieces_cnt


checks :: ([Int], [(Int, Int, [Int])]) -> Int
checks (pieces, problems)
    | nProblems == solvable + nonSolvable = solvable
    | otherwise = error "non trivial"
  where
    solvable    = length (filter hasSolution problems)
    nonSolvable = length (filter (noSolution pieces) problems)
    nProblems   = length problems


presentsP :: Parser ([Int], [(Int, Int, [Int])])
presentsP = liftA2 (,) (try tileP `sepEndBy1` space) (problemP `sepEndBy1` space)
  where
    tileP = do
        (_num :: Int) <- lexeme (decimal <* char ':') <* space
        rows          <- rowP `sepEndBy1` space
        return (length (concatMap (filter id) rows))

    rowP = some $ (char '.' >> pure False) <|> (char '#' >> pure True)

    problemP = do
        (x, y) <- liftA2 (,) (decimal <* char 'x') (decimal <* char ':')
        nums   <- blanksP >> some (lexeme decimal)
        return (x, y, nums)


main :: IO ()
main = applyInput1 presentsP checks