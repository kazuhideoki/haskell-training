module Main where

main :: IO ()
main = do
  arg <- readLn :: IO Int
  let result = exe arg
  print result

-- 文字列 `o` を引数分挿入する Loooong
exe :: Int -> String
exe n = "L" ++ replicate n 'o' ++ "ng"
