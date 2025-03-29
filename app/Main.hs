-- C - Make it Forest
-- https://atcoder.jp/contests/abc399/tasks/abc399_c
module Main where

import Control.Monad (replicateM)
import Data.Functor ((<&>))

-- sort する -> 順位つける -> 戻す
main :: IO ()
main = do
  -- 例: [10, 10] [頂点, 辺の数]
  [n, m] <- (getLine <&> map read . words) :: IO [Int]
  -- 例: [[1, 2], [3, 4], [5, 6]] [辺の始点, 辺の終点]
  vertexes <- replicateM m (getLine <&> map read . words) :: IO [[Int]]
  print [n, m]
  print vertexes
