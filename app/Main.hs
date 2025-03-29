-- B - Ranking with Ties
-- https://atcoder.jp/contests/abc399/tasks/abc399_b
module Main where

import Data.Function ((&))
import Data.Functor ((<&>))

main :: IO ()
main = do
  -- 例: 4
  n <- (getLine <&> read) :: IO Int
  -- 例: [1, 2, 3, 4]
  s <- (getLine <&> map read . words) :: IO [Int]
  print n
  print s
