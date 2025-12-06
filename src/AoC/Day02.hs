module AoC.Day02 (solution) where

import Data.List.Split (splitWhen)
import qualified Utils.Utils as Utils

data PasswordPolicy = PasswordPolicy {minCount :: Int, maxCount :: Int, letter :: Char}

parsing :: [String] -> [(PasswordPolicy, String)]
parsing = map (toPasswordPolicy . splitInput)
  where
    splitInput :: String -> [String]
    splitInput = splitWhen (\c -> c == '-' || c == ':' || c == ' ')

    toPasswordPolicy :: [String] -> (PasswordPolicy, String)
    toPasswordPolicy [minC, maxC, l, _, pwd] =
      ( PasswordPolicy
          { minCount = read minC,
            maxCount = read maxC,
            letter = head l
          },
        pwd
      )
    toPasswordPolicy _ = error "Not valid password policy format."

validPassword :: PasswordPolicy -> String -> Bool
validPassword policy password
  | minCount policy <= count password && count password <= maxCount policy = True
  | otherwise = False
  where
    count = length . filter (== letter policy)

part1 :: Utils.SolutionSingle
part1 = length . filter (uncurry validPassword) . parsing

validPasswordPart2 :: PasswordPolicy -> String -> Bool
validPasswordPart2 policy password =
  (minChar == letter policy) /= (maxChar == letter policy)
  where
    minChar = password !! (minCount policy - 1)
    maxChar = password !! (maxCount policy - 1)

part2 :: Utils.SolutionSingle
part2 = length . filter (uncurry validPasswordPart2) . parsing

solution :: Utils.Solution
solution = (part1, part2)
