-- B - N-choice question
-- https://atcoder.jp/contests/adt_easy_20250319_3/tasks/abc300_a
module Main where

import GHC.OldList (elemIndex)

main :: IO ()
main = do
  line1 <- getLine
  line2 <- getLine
  let intList = map read (words line1) :: [Int]
      nums = tail intList
      answers = map read (words line2) :: [Int]

  let result = case elemIndex (sum nums) answers of
        Just idx -> idx + 1
        Nothing -> error "Unable to find answer"
  print result
