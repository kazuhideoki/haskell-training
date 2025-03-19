module Main where

main :: IO ()
main = do
  line1 <- getLine
  line2 <- getLine
  let numbers1 = map read (words line1) :: [Int]
      numbers2 = map read (words line2) :: [Int]
      sum1 = sum numbers1
      sum2 = sum numbers2
      result = sum1 - sum2 + 1
  print result
