-- A - Hamming Distance
-- https://atcoder.jp/contests/abc399/tasks/abc399_a
module Main where

import Data.Function ((&))
import Data.Functor ((<&>))

main :: IO ()
main = do
  n <- (getLine <&> read) :: IO Int
  s <- getLine
  t <- getLine
  let result = zipWith (/=) s t & filter id & length
  print result
