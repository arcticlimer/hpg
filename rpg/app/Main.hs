module Main where

import Lib
import Game (createPlayer)

main :: IO ()
main = do
  player <- createPlayer
  print player
