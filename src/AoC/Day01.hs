module AoC.Day01 (solution) where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Utils.Utils as Utils

part1 :: Utils.SolutionSingle
part1 = helper Set.empty . map read
  where
    helper :: Set.Set Int -> [Int] -> Int
    helper _ [] = 0
    helper seen (x : xs)
      | Set.member (2020 - x) seen = x * (2020 - x)
      | otherwise = helper (Set.insert x seen) xs

part2 :: Utils.SolutionSingle
part2 = findX . map read
  where
    findX :: [Int] -> Int
    findX [] = 0
    findX (x : xs)
      | Maybe.isNothing res = findX xs
      | otherwise = Maybe.fromJust res
      where
        res = iter2 x xs

    iter2 :: Int -> [Int] -> Maybe Int
    iter2 _ [] = Nothing
    iter2 x (y : ys)
      | Maybe.isNothing res = iter2 x ys
      | otherwise = res
      where
        res = findZ (x, y) ys

    findZ :: (Int, Int) -> [Int] -> Maybe Int
    findZ _ [] = Nothing
    findZ (x, y) (z : zs) = if z == (2020 - x - y) then Just $ x * y * z else findZ (x, y) zs

solution :: Utils.Solution
solution = (part1, part2)
