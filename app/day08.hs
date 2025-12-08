module Main where

import           AoC                  (applyInput1, decimal)
import           Control.Applicative  (liftA3)
import           Control.Arrow        ((>>>))
import qualified Data.DisjointSet     as DS
import           Data.List            (sortBy)
import qualified Data.PQueue.Min      as PQ
import           Text.Megaparsec      (sepEndBy1)
import           Text.Megaparsec.Char (char, space)


type Coord = (Int, Int, Int)


fst3 :: Coord -> Int
fst3 (a, _, _) = a


distance :: Coord -> Coord -> Int
distance (x1, y1, z1) (x2, y2, z2) =
    (x1 - x2) ^^^ 2 + (y1 - y2) ^^^ 2 + (z1 - z2) ^^^ 2
  where
    a ^^^ (b :: Int) = a ^ b


getLastConnection :: DS.DisjointSet Coord -> PQ.MinQueue (Int, Coord, Coord) -> Int
getLastConnection ds conns =
    case conns of
        PQ.Empty ->
            error "no more connections"
        (_, a, b) PQ.:< cs ->
            let newDs = DS.union a b ds
            in if DS.sets newDs == 1
                then fst3 a * fst3 b
                else getLastConnection newDs cs


get3Largest :: DS.DisjointSet Coord -> Int
get3Largest =
    DS.toLists
    >>> map length
    >>> sortBy (flip compare)
    >>> take 3
    >>> product


solve :: [Coord] -> (Int, Int)
solve boxes =
    let p1DS = foldl' (\acc (_, a, b) -> DS.union a b acc) ds0 (PQ.take 1000 pairQ)
    in (get3Largest p1DS, getLastConnection p1DS (PQ.drop 1000 pairQ))
  where
    pairQ = PQ.fromList [(distance b1 b2, b1, b2) | b1 <- boxes, b2 <- boxes, b1 < b2]
    ds0   = DS.fromLists (map (:[]) boxes)


main :: IO ()
main = applyInput1 (coord3P `sepEndBy1` space) solve
  where
    coord3P = liftA3 (,,) (decimal <* char ',') (decimal <* char ',') decimal