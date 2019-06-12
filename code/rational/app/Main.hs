module Main where

import           Lib
import           Text.Trifecta

main :: IO ()
main = do
  print $ p parseRational "1/1"
  -- print $ p parseRational "1/0"
  print $ p parseRational' "1/1"
  print $ p parseRational' "1/0"
