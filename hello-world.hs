{- stack
  script
  --resolver lts-8.11
-}
{-# LANGUAGE OverloadedStrings #-}


choices = [
  ("English", "Hello World"),
  ("German", "Hallo Welt"),
  ("Chinese", "你好世界"),
  ("Japanese", "こんにちは世界"),
  ("Esperanto", "Saluton mondo")
  ]


main =
  putStrLn $ snd $ choices !! 0
