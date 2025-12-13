module Main where

import           AoC                  (applyInput, blanksP, decimal, lexeme)
import           Control.Applicative  (liftA3)
import           Data.Function        ((&))
import           Data.List            (find, sortOn, subsequences, sortOn)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (catMaybes, fromMaybe)
import           Text.Megaparsec      ((<|>), between, sepBy1, sepEndBy1, some)
import           Text.Megaparsec.Char (char, space)


parity :: [Int] -> [Bool]
parity = map odd


applyButtons :: (a -> a) -> [a] -> [[Int]] -> [a]
applyButtons f = foldl' (applyButton 0)
  where
    applyButton _ [] (_:_) = error "no index"
    applyButton _ jtg []   = jtg
    applyButton i (j:js) (b:bs)
        | i < b  = j : applyButton (i+1) js (b:bs)
        | i == b = f j : applyButton (i+1) js bs
        | otherwise = error "index"


solveMachineP1 :: ([Bool], [[Int]], [Int]) -> Int
solveMachineP1 (lights, buttons, _) =
    subsequences buttons
    & sortOn length
    & find ((==lights) . applyButtons not allOff)
    & maybe (error "no solution") length
  where
    allOff = replicate (length lights) False


minimize :: M.Map [Bool] [([Int], Int)] -> [Int] -> Maybe Int
minimize m joltages
    | any  (<0) joltages = Nothing
    | all (==0) joltages = Just 0
    | otherwise = do
        opts <- m M.!? parity joltages
        minMaybe $ map checkOpt opts
  where
    minMaybe l = case catMaybes l of
        []   -> Nothing
        opts -> Just $ minimum opts

    checkOpt (changes, cost) =
          zipWith (-) joltages changes
        & map (`div` 2)
        & minimize m
        & fmap (\c -> cost + c * 2)


solveMachineP2 :: ([Bool], [[Int]], [Int]) -> Int
solveMachineP2 (_, buttons, joltages) = fromMaybe (-1) $ minimize m joltages
  where
    all0 = replicate (length joltages) 0

    m = subsequences buttons
      & foldl' insertPattern M.empty

    insertPattern mp btns =
        let res    = applyButtons (+1) all0 btns
        in M.insertWith (++) (parity res) [(res, length btns)] mp


solve :: (([Bool], [[Int]], [Int]) -> Int) -> [([Bool], [[Int]], [Int])] -> Int
solve solver = sum . map solver


main :: IO ()
main = applyInput (machineP `sepEndBy1` space) (solve solveMachineP1) (solve solveMachineP2)
  where
    machineP = liftA3 (,,) (lexeme lightsP) (buttonP `sepEndBy1` blanksP) joltageP

    lightsP = between (char '[') (char ']') (some lightP)

    lightP = (char '.' >> pure False) <|> (char '#' >> pure True)

    buttonP = between (char '(') (char ')') (decimal `sepBy1` char ',')

    joltageP = between (char '{') (char '}') (decimal `sepBy1` char ',')