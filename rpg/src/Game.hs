{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Game (createPlayer, createEnemy) where

import Data.Char (toLower)
import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)

data PlayerClass = Mage | Warrior | Archer
  deriving (Show)

data Player = Player
  { hp :: Int,
    mp :: Int,
    name :: String,
    _class :: PlayerClass
  }
  deriving (Show)

data Enemy = Enemy
  { hp :: Int,
    name :: String
  }
  deriving (Show)

printNoLn :: String -> IO ()
printNoLn a = do
  putStr a
  hFlush stdout

prompt = "-> "

ask :: String -> String -> (String -> Maybe a) -> IO a
ask message prompt parser = do
  printNoLn $ printf "%s %s" message prompt
  line <- getLine
  maybe recurse return (parser line)
  where
    recurse = ask message prompt parser

createPlayer :: IO Player
createPlayer = do
  hp <- ask "Player HP" prompt readMaybe
  mp <- ask "Player MP" prompt readMaybe
  name <- ask "Player Name" prompt Just
  _class <- ask "Player class" prompt parsePlayerClass
  pure $ Player {hp, mp, name, _class}

parsePlayerClass :: String -> Maybe PlayerClass
parsePlayerClass s =
  case lower of
    "mage" -> Just Mage
    "warrior" -> Just Warrior
    "archer" -> Just Archer
    _ -> Nothing
  where
    lower = map toLower s

createEnemy :: Enemy
createEnemy =
  Enemy
    { hp = 100,
      name = "Foo"
    }
