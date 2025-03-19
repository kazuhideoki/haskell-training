-- A - 12435
-- https://atcoder.jp/contests/adt_easy_20250319_3/tasks/abc390_a
-- 前後で 1 の差が2つあるかどうか？
module Main where

main :: IO ()
main = do
  line <- getLine
  let intList = map read (words line) :: [Int]
  -- let result = checkAscCount intList
  let result = checkPatterns (generate4Patterns intList)
  putStrLn result

-- 「アプローチ1」 直感的にやったロジック -> テストでは部分的正解 🔺
checkAscCount :: (Num a, Eq a) => [a] -> String
checkAscCount (x1 : x2 : x3 : x4 : x5 : _) =
  let isFirst = x2 - x1 == 1
      isSecond = x3 - x2 == 1
      isThird = x4 - x3 == 1
      isFourth = x5 - x4 == 1
      conditions = [isFirst, isSecond, isThird, isFourth]
   in if sum (map fromEnum conditions) == 2
        then "Yes"
        else "No"
checkAscCount _ = "No" -- 5要素未満のリストの場合

-- 「アプローチ2」 実際に並べかえた4パターンで、1つが昇順となるかを判定する 🔵
-- 1. index を指定して、その後ろと入れ替える
-- インデックスiから始まる2要素を反転させる
reverseTwo :: Int -> [a] -> [a]
reverseTwo i xs
  | i < 0 || i + 1 >= length xs = xs -- 範囲外チェック
  | otherwise =
      let (before, rest) = splitAt i xs
          toReverse = take 2 rest
          after = drop 2 rest
       in before ++ reverse toReverse ++ after

-- 2. 4パターン生成する
generate4Patterns :: [Int] -> [[Int]]
generate4Patterns xs = [reverseTwo i xs | i <- [0 .. 3]]

-- 3. 4パターンをチェックし、1つ昇順のものがあるかチェック
checkPatterns :: [[Int]] -> String
checkPatterns patterns =
  let equalCount = map (\pattern -> pattern == [1 .. 5]) patterns
   in if sum (map fromEnum equalCount) == 1
        then "Yes"
        else "No"
