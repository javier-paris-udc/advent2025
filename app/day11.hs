module Main where

import           AoC                  (applyInput, lexeme)
import qualified Data.HashMap.Strict  as Map
import           Data.MemoTrie        (memoFix)
import           Text.Megaparsec.Char (char, letterChar, space)
import           Text.Megaparsec      (many, sepEndBy1, some)


pathsThroughDacFft :: String -> String -> Map.HashMap String [String] -> Int
pathsThroughDacFft from to m = let (res, _, _, _) = memoFix findWays from in res
  where
    findWays go now
        | now == to = (0, 0, 0, 1)
        | otherwise =
            case m Map.!? now of
                Nothing ->
                    (0, 0, 0, 0)
                Just l ->
                    let (both, dac, fft, none) = sumAll (map go l)
                    in case now of
                        "dac" -> (both + fft, dac + none, 0, 0)
                        "fft" -> (both + dac, 0, fft + none, 0)
                        _     -> (both, dac, fft, none)

    sumAll = foldl' add4 (0, 0, 0, 0)

    add4 (a1, b1, c1, d1) (a2, b2, c2, d2) = (a1 + a2, b1 + b2, c1 + c2, d1 + d2)


paths :: String -> String -> Map.HashMap String [String] -> Int
paths from to m
     | from == to = 1
     | otherwise  =
        case m Map.!? from of
            Nothing -> 0
            Just l -> sum $ map (\s -> paths s to m) l


solveP2 :: Map.HashMap String [String] -> Int
solveP2 = pathsThroughDacFft "svr" "out"


solveP1 :: Map.HashMap String [String] -> Int
solveP1 = paths "you" "out"


main :: IO ()
main = applyInput (Map.fromList <$> (deviceP `sepEndBy1` space)) solveP1 solveP2
  where
    deviceP = liftA2 (,) (lexeme (nameP <* char ':')) (many (lexeme nameP))

    nameP = some letterChar