{- stack
  script
  --resolver lts-8.11
  --package random
-}
{-# LANGUAGE OverloadedStrings #-}

import System.Random


choices :: [(String, String)]
choices = [
  ("English", "Hello World"),
  ("German", "Hallo Welt"),
  ("Chinese", "你好世界"),
  ("Japanese", "こんにちは世界"),
  ("Esperanto", "Saluton mondo")
  ]

randomIndex :: Int -> StdGen -> (Int, StdGen)
randomIndex n g =
  randomR (0, n) g


main :: IO ()
main = do
  gen <- getStdGen
  let n = length choices
  let (index, _) = randomIndex (n - 1) gen
  -- putStrLn $ show index
  let (lang, phrase) = choices !! index
  putStrLn $ phrase ++ " (" ++ lang ++ ")"
