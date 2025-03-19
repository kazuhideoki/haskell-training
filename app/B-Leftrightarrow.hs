-- B - Leftrightarrowf
-- https://atcoder.jp/contests/adt_easy_20250319_2/tasks/abc345_a
module Main where

main :: IO ()
main = do
  str <- getLine
  let chars = map (: []) str :: [String]
  let isHeadArrow = head chars == "<"
  let isTailArrow = last chars == ">"
  let isMiddleEqual =
        let middle = if length chars >= 3 then init (tail chars) else []
         in all (== "=") middle
  let result = if isHeadArrow && isTailArrow && isMiddleEqual then "Yes" else "No"
  putStrLn result
