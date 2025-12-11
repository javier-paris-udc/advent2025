module Main where

import Text.Megaparsec (sepEndBy1, between, sepBy1, (<|>), some)
import Control.Applicative (liftA3)
import AoC (applyInput, blanksP, lexeme, decimal)
import Text.Megaparsec.Char (char, space)
import Dijkstra (dijkstra)
import Data.Function ((&))


applyButtonsP2 :: [Int] -> [[Int]] -> [Int] -> [([Int], Int)]
applyButtonsP2 joltageGoal buttons joltage =
    buttons
    & map (applyButton 0 joltage)
    & filter (and . zipWith (>=) joltageGoal)
    & map (,1)
  where
    applyButton _ [] (_:_) = error "no joltage"
    applyButton _ jtg []   = jtg
    applyButton i (j:js) (b:bs)
        | i < b  = j : applyButton (i+1) js (b:bs)
        | i == b = (j + 1) : applyButton (i+1) js bs
        | otherwise = error "index"


solveMachineP2 :: ([Bool], [[Int]], [Int]) -> Int
solveMachineP2 (_, buttons, joltage) =
    case dijkstra (replicate (length joltage) 0) (== joltage) (applyButtonsP2 joltage buttons) of
        Nothing -> error "no solution"
        Just path -> length path - 1


applyButtonsP1 :: [[Int]] -> [Bool] -> [([Bool], Int)]
applyButtonsP1 buttons lights = map ((,1) . applyButton 0 lights) buttons
  where
    applyButton _ ls []    = ls
    applyButton _ [] (_:_) = error "no lights"
    applyButton i (l:ls) (b:bs)
        | i <  b = l : applyButton (i+1) ls (b:bs)
        | i == b = not l : applyButton (i+1) ls bs
        | otherwise = error "index"


solveMachineP1 :: ([Bool], [[Int]], [Int]) -> Int
solveMachineP1 (lights, buttons, _) =
    case dijkstra (replicate (length lights) False) (== lights) (applyButtonsP1 buttons) of
        Nothing -> error "no solution"
        Just path -> length path - 1



solveP2 :: [([Bool], [[Int]], [Int])] -> Int
solveP2 = solveMachineP2 . head


solveP1 :: [([Bool], [[Int]], [Int])] -> Int
solveP1 = sum . map solveMachineP1


main :: IO ()
main = applyInput (machineP `sepEndBy1` space) solveP1 solveP2
  where
    machineP = liftA3 (,,) (lexeme lightsP) (buttonP `sepEndBy1` blanksP) joltageP

    lightsP = between (char '[') (char ']') (some lightP)

    lightP = (char '.' >> pure False) <|> (char '#' >> pure True)

    buttonP = between (char '(') (char ')') (decimal `sepBy1` char ',')

    joltageP = between (char '{') (char '}') (decimal `sepBy1` char ',')