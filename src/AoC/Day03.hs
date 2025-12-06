module AoC.Day03 (solution) where

import qualified Data.Bifunctor as Bifunctor
import qualified Utils.Grid as Grid
import qualified Utils.Utils as Utils

checkSlope :: (Int, Int) -> Grid.Grid Char -> Int
checkSlope slope grid =
  let positions = stepN (Grid.height grid) (0, 0)
      positionsNoOverflow = map (Bifunctor.second (`mod` Grid.width grid)) positions
      cellsHit = map (`Grid.getCell` grid) positionsNoOverflow
   in length $ filter (== '#') cellsHit
  where
    stepN :: Int -> (Int, Int) -> [(Int, Int)]
    stepN height (prevX, prevY)
      | height <= prevX = []
      | otherwise = (prevX, prevY) : stepN height (Bifunctor.bimap (prevX +) (prevY +) slope)

part1 :: Utils.SolutionSingle
part1 = checkSlope (1, 3) . Grid.fromGridList

part2 :: Utils.SolutionSingle
part2 input =
  let grid = Grid.fromGridList input
      slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
      treesHit = map (`checkSlope` grid) slopes
   in product treesHit

solution :: Utils.Solution
solution = (part1, part2)
