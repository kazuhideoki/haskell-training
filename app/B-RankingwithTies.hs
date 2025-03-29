-- B - Ranking with Ties
-- https://atcoder.jp/contests/abc399/tasks/abc399_b
module Main where

import Data.Function ((&))
import Data.Functor ((<&>))

-- sort する -> 順位つける -> 戻す
main :: IO ()
main = do
  -- 例: 4
  n <- (getLine <&> read) :: IO Int
  -- 例: [1, 2, 3, 4]
  s <- (getLine <&> map read . words) :: IO [Int]
  let initialPoints = zip s (replicate n 0)
      (_, result) = decideRank (1, initialPoints)
      ranks = map (show . snd) result & unwords
   in putStrLn ranks

-- r -> (point, rank) -> (point, rank)
decideRank :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
decideRank (r, points) =
  let maxPoint = points & filter (\(_, rank) -> rank == 0) & map fst & maximum
      result = points & map (\(point, rank) -> if point == maxPoint then (point, r) else (point, rank))
      newR = result & filter ((/= 0) . snd) & length & (+) 1
   in if newR > length points then (newR, result) else decideRank (newR, result)
