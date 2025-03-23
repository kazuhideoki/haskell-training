-- C - Let's Get a Perfect Score
-- https://atcoder.jp/contests/adt_easy_20250319_3/tasks/abc282_b
module Main where

import Control.Monad (replicateM)
import Data.Function ((&))

main :: IO ()
main = do
  line1 <- getLine
  let intList = map read (words line1) :: [Int]
      numOfAnswers = head intList
  answers <- replicateM numOfAnswers getLine

  let result = numOfGettingCorrectAnswersByTwo answers

  print result

numOfGettingCorrectAnswersByTwo :: [String] -> Int
numOfGettingCorrectAnswersByTwo answers =
  pairs answers & filter canSolve & length

-- 重複しないようなペアを生成する
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs

-- 'o' が含まれるペアを生成する
canSolve :: (String, String) -> Bool
canSolve (x, y) =
  let charPairs = zip x y
   in all (\(a, b) -> a == 'o' || b == 'o') charPairs
