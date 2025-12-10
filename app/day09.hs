module Main where

import AoC                  (applyInputWith, decimal)
import Data.Function        (on)
import Data.List            (find, sort, sortBy)
import Data.Maybe           (mapMaybe)
import Text.Megaparsec      (sepEndBy1)
import Text.Megaparsec.Char (char, space)


type Coord = (Int, Int)

data Segment =
    Segment {fixed :: Int
            ,start :: Int
            ,end :: Int
    } deriving (Show, Eq, Ord)


area :: (Coord, Coord) -> Int
area ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)


segmentToInterval :: Segment -> (Int, Int)
segmentToInterval sg = (start sg, end sg)


-- TODO : simplify this 3 functions as two passes adding col and taking away v


defuse :: [(Int, Int)] -> [Segment] -> [(Int, Int)]
defuse [] []  = []
defuse [] _   = error "defuse taking away from nothing"
defuse sgs [] = sgs
defuse ((vSt, vEnd) : valids) (v:vs)
    | vSt == start v && vEnd == end v = defuse valids vs
    | vEnd < start v = (vSt, vEnd) : defuse valids (v : vs)
    | vSt == start v = defuse ((end v, vEnd) : valids) vs
    | vEnd == end v  = defuse ((vSt, start v) : valids) vs
    | vSt < start v && end v < vEnd = (vSt, start v) : defuse ((end v, vEnd):valids) vs
    | otherwise = error "defusing an area not in valid region"


fuse :: [(Int, Int)] -> [Segment] -> [(Int, Int)]
fuse [] col = map segmentToInterval col
fuse sgs [] = sgs
fuse ((vSt, vEnd):valids) (v:vs)
    | end v < vSt = segmentToInterval v : fuse ((vSt, vEnd):valids) vs
    | vEnd < start v = (vSt, vEnd) : fuse valids (v:vs)
    | end v == vSt = fuse ((start v, vEnd) : valids) vs
    | start v == vEnd = fuse ((vSt, end v) : valids) vs
    | otherwise = (vSt, vEnd) : fuse valids vs


adjustValid :: [(Int, Int)] -> [Segment] -> [Segment] -> [(Int, Int)]
adjustValid [] col [] = map segmentToInterval col
adjustValid [] _   _  = error "prev nonempty with valid empty"
adjustValid ((vSt, vEnd):valids) (v:vs) (p:ps)
    | end v   <  vSt  = (start v, end v) : adjustValid ((vSt, vEnd) : valids) vs (p:ps)
    | end v   == vSt  = adjustValid ((start v, vEnd) : valids) vs (p:ps)
    | start v == vEnd = adjustValid ((vSt, end v) : valids) vs (p:ps)
    | start p == vSt && end p == vEnd = adjustValid valids (v:vs) ps
    | start p == vSt && end p <  vEnd = adjustValid ((end p, vEnd):valids) (v:vs) ps
    | end p == vEnd && start p > vSt  = adjustValid ((vSt, start p):valids) (v:vs) ps
    | vEnd < start p && vEnd < start v = (vSt, vEnd) : adjustValid valids (v:vs) (p:ps)
    | start p > vSt && end p < vEnd = (vSt, start p) : adjustValid ((end p, vEnd) : valids) (v:vs) ps
    | otherwise = error "unexpected case"
adjustValid valids col [] = fuse valids col
adjustValid valids [] prevs = defuse valids prevs


getOff :: [(Int, Int)] -> [Segment] -> [Segment]
getOff [] _ = []
getOff _ [] = []
getOff ((vSt, vEnd):valids) (v:vs)
    | end v <= vSt = getOff ((vSt, vEnd) : valids) vs
    | start v >= vEnd = getOff valids (v:vs)
    | vSt == start v && vEnd == end v = v : getOff valids vs
    | vSt == start v = v : getOff ((end v, vEnd):valids) vs
    | vEnd == end v = v : getOff valids vs
    | vSt < start v && end v < vEnd = v : getOff ((vSt, vEnd) : valids) vs
    | otherwise = error "getOff"


checkFuse :: [(Int, Int)] -> [(Int, Int)]
checkFuse [] = []
checkFuse [i] = [i]
checkFuse ((st1, end1):(st2, end2):rest)
    | end1 == st2 = checkFuse ((st1, end2):rest)
    | otherwise = (st1, end1) : checkFuse ((st2, end2):rest)


buildIntervals :: [Segment] -> [(Int, [(Int, Int)])]
buildIntervals segments = aux segments [] []
  where
    aux [] [] _ = []
    aux [] (p:prev) valid = [(fixed p + 1, adjustValid valid [] (p:prev))]
    aux (s:ss) prev valid
        | isPrevBefore prev s =
            let newValid = checkFuse $ adjustValid valid [] prev
            in (getCol prev, newValid) : aux (s:ss) [] newValid
        | otherwise =
            let (col, rest) = span (\s2 -> fixed s == fixed s2) (s:ss)
                newValid = checkFuse $ adjustValid valid col prev
                offNext  = getOff valid col
            in if valid == newValid then aux rest offNext valid
               else (fixed s, newValid) : aux rest offNext newValid

    isPrevBefore [] _ = False
    isPrevBefore (p:_) s = fixed p < fixed s

    getCol [] = error "no col"
    getCol (p:_) = fixed p

-- We only build the vertical segments, because they completely define the figure
buildSegments :: [Coord] -> [Segment]
buildSegments redTiles = sort $ mapMaybe coordsToSegment tilePath
  where
    tilePath   = zip redTiles (drop 1 redTiles ++ take 1 redTiles)
    coordsToSegment ((x1, y1), (x2, y2))
        | x1 == x2  = Just $ Segment { fixed = x1, start = min y1 y2, end = max y1 y2 }
        | otherwise = Nothing


isValid :: [(Int, [(Int, Int)])] -> (Coord, Coord) -> Bool
isValid iVals ((x1, y1), (x2, y2)) =
    all (inside y1 y2) relevantIVals
  where
    relevantIVals = findValidIvals iVals x1 x2

    findValidIvals ((c1, l1) : (c2, l2) : ivs) a1 a2
        | c1 < a1 && c2 > a1 = (c1, l1):findValidIvals ((c2, l2) : ivs) a1 a2
        | c1 < a1 = findValidIvals ((c2, l2) : ivs) a1 a2
        | c1 >= a1 && c1 <= a2 = (c1, l1):findValidIvals ((c2, l2) : ivs) a1 a2
        | otherwise = []
    findValidIvals [(c,l)] _ a2
        | c <= a2 = [(c, l)]
        | otherwise = []
    findValidIvals [] _ _ = []

    inside a b (_, ranges) = any (\(st, end) -> st <= a && a <= end && st <= b && b <= end) ranges



solveP2 :: [Coord] -> [(Coord, Coord)] -> Int
solveP2 redTiles pairs =
    case find (isValid iVals) pairs of
        Nothing -> error "no valid square"
        Just p  -> area p
  where
    iVals = buildIntervals $ buildSegments redTiles


solveP1 :: [(Coord, Coord)] -> Int
solveP1 pairs =
    case pairs of
        [] -> error "no coords"
        rect:_ -> area rect


solve :: [(Int, Int)] -> IO ()
solve redTiles = do
    let pairs = sortBy (flip compare `on` area) [(p1, p2) | p1 <- redTiles, p2 <- redTiles, p1 < p2]
    print $ solveP1 pairs
    print $ solveP2 redTiles pairs


main :: IO ()
main = applyInputWith (liftA2 (,) (decimal <* char ',') decimal `sepEndBy1` space) solve