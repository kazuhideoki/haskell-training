-- E - World Tour Finals
-- https://atcoder.jp/contests/adt_easy_20250319_3/tasks/abc323_c
module Main where

import Control.Monad (replicateM)
import Data.Function ((&))
import Data.List (sortBy)

main :: IO ()
main = do
  line1 <- getLine
  line2 <- getLine
  let list1 = line1 & words & map read :: [Int]
      playerCount = head list1
      _problemCount = head $ tail list1
      scores = line2 & words & map read :: [Int]
  progresses <- replicateM playerCount getLine
  let topScore = maximum $ zipWith (`calcScore` scores) progresses [0 ..]
  let counts =
        zipWith
          ( \progress bonus ->
              calcMinimumRequiredSolving progress scores topScore bonus
          )
          progresses
          [0 ..]
   in do
        mapM_ print counts

toBools :: [String] -> [Bool]
toBools = map (== "o")

-- 現在の最高得点を取得 (progress -> scores -> bounusScore)
calcScore :: String -> [Int] -> Int -> Int
calcScore _ [] _ = 0
calcScore [] _ _ = 0
calcScore xs ys bonus =
  zip xs ys
    & filter (\(x, _) -> x == 'o')
    & map snd
    & sum
    & (+) bonus

-- トップを越えるために必要な最低の正解数を取得
-- (progress -> scores -> topScore -> bonusScore -> 正解数)
-- 1. 正解してない問題からスコアの大きい順に足していく
-- 2. スコアがrequiedScore に達したら足された数が出力
calcMinimumRequiredSolving :: String -> [Int] -> Int -> Int -> Int
calcMinimumRequiredSolving progress scores topScore bonusScore =
  let currentScore = calcScore progress scores bonusScore
      notSolvingScores = zip progress scores & filter (\(x, _) -> x == 'x') & map snd & sortBy (flip compare)
   in if currentScore == topScore
        then 0
        else
          snd $
            foldl
              ( \(totalScore, count) score ->
                  if totalScore <= topScore
                    then (totalScore + score, count + 1)
                    else (totalScore, count)
              )
              (currentScore, 0)
              notSolvingScores
