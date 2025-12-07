module Main where

import           AoC                  (applyInput)
import qualified Data.IntMap.Strict   as Map
import qualified Data.IntSet          as Set
import           Data.List            (elemIndex, elemIndices, mapAccumL)
import           Data.Maybe           (fromMaybe)
import           Text.Megaparsec      ((<|>), sepEndBy1, some)
import           Text.Megaparsec.Char (char, space)


solveP2 :: (Int, [Set.IntSet]) -> Int
solveP2 (st, sets) = sum $ foldl' advance (Map.singleton st 1) sets
  where
    advance posMap manifoldLine =
        Map.foldlWithKey' (checkManifold manifoldLine) Map.empty posMap

    checkManifold manifoldLine m pos times
        | pos `Set.member` manifoldLine =
            Map.insertWith (+) (pos - 1) times (Map.insertWith (+) (pos + 1) times m)
        | otherwise =
            Map.insertWith (+) pos times m


solveP1 :: (Int, [Set.IntSet]) -> Int
solveP1 (st, sets) = sum $ snd $ mapAccumL advance (Set.singleton st) sets
  where
    advance posSet manifoldLine =
        Set.foldl' (checkManifold manifoldLine) (Set.empty, 0) posSet

    checkManifold manifoldLine (s, ops) pos
        | pos `Set.member` manifoldLine =
            (Set.insert (pos - 1) $ Set.insert (pos + 1) s, ops + 1)
        | otherwise =
            (Set.insert pos s, ops)


main :: IO ()
main = applyInput manifoldP solveP1 solveP2
  where
    manifoldP = do
        st <- stP <* space
        field <- lineP `sepEndBy1` space
        return (st, field)

    stP = do
        firstLine <- some $ char '.' <|> char 'S'
        return $ fromMaybe (error "no start position") $ elemIndex 'S' firstLine

    lineP = do
        manifoldLine <- some $ char '.' <|> char '^'
        return $ Set.fromList $ elemIndices '^' manifoldLine