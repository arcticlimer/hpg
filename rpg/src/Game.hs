{-# LANGUAGE NamedFieldPuns #-}

module Game (main) where

import Data.Char (toLower)
import System.IO
import System.Random (Random (randomR, randoms), RandomGen (genRange), StdGen, newStdGen, randomIO)
import Text.Printf (printf)
import Text.Read (readMaybe)

data PlayerClass = Mage | Warrior | Archer
  deriving (Show)

data Player = Player
  { hp :: Int,
    mp :: Int,
    atk :: Int,
    name :: String,
    cls :: PlayerClass
  }
  deriving (Show)

data Enemy = Enemy
  { eHp :: Int,
    eAtk :: Int,
    eName :: String
  }
  deriving (Show)

prompt = " -> "

printNoLn :: String -> IO ()
printNoLn a = do
  putStr a
  hFlush stdout

ask :: String -> String -> (String -> Maybe a) -> IO a
ask message prompt parser = do
  printNoLn $ printf "%s%s" message prompt
  line <- getLine
  maybe recurse return (parser line)
  where
    recurse = ask message prompt parser

data UserAction = Attack | Block

class Fighter a where
  takeDamage :: Int -> a -> Maybe a
  getDamage :: a -> Int

instance Fighter Enemy where
  takeDamage dmg enemy = if life > 0 then Just enemy else Nothing
    where
      life = eHp $ decreaseEnemyHealth dmg enemy
  getDamage = eAtk

instance Fighter Player where
  takeDamage dmg player = if life > 0 then Just player else Nothing
    where
      life = hp $ decreasePlayerHealth dmg player
  getDamage = atk

decreaseEnemyHealth :: Int -> Enemy -> Enemy
decreaseEnemyHealth v e = e {eHp = eHp e - v}

decreasePlayerHealth :: Int -> Player -> Player
decreasePlayerHealth v p = p {hp = hp p - v}

attack :: Fighter f => Fighter g => f -> g -> Maybe g
attack f1 = takeDamage (getDamage f1)

fight :: Player -> Enemy -> IO (Either Enemy Player)
fight player enemy = do
  action <- ask "Take your action (attack/block)" prompt parsePlayerAction
  let result = player `attack` enemy
  putStrLn "You have hit the enemy!"

  -- TODO: Log enemy hit before it happens
  case result of
    Just hittedEnemy -> case enemy `attack` player of
      Just hittedPlayer -> putStrLn "Enemy hit you!" >> fight hittedPlayer hittedEnemy
      Nothing -> pure $ Left enemy
    Nothing -> pure $ Right player

parsePlayerAction :: String -> Maybe UserAction
parsePlayerAction input = case lower of
  "attack" -> Just Attack
  "block" -> Just Block
  _ -> Nothing
  where
    lower = toLower <$> input

main :: IO ()
main = do
  player <- createPlayer
  enemy <- createEnemy
  winner <- startBattle player enemy
  let msg = case winner of
        Left _enemy -> "Enemy wins the fight, game over..."
        Right _player -> "You won the fight!"
  putStrLn msg

startBattle :: Player -> Enemy -> IO (Either Enemy Player)
startBattle p e =
  putStrLn "You have encountered an enemy! The battle has begun!"
    >> fight p e

createPlayer :: IO Player
createPlayer = do
  hp <- ask "Player HP" prompt readMaybe
  mp <- ask "Player MP" prompt readMaybe
  name <- ask "Player Name" prompt Just
  atk <- ask "Player attack points" prompt readMaybe
  cls <- ask "Player class" prompt parsePlayerClass
  pure $ Player {hp, mp, name, cls, atk}

parsePlayerClass :: String -> Maybe PlayerClass
parsePlayerClass s =
  case lower of
    "mage" -> Just Mage
    "warrior" -> Just Warrior
    "archer" -> Just Archer
    _ -> Nothing
  where
    lower = toLower <$> s

createEnemy :: IO Enemy
createEnemy = do
  g <- newStdGen
  let (hp, ng) = randomR (100, 150) g :: (Int, StdGen)
  let (atk, _) = randomR (20, 30) ng :: (Int, StdGen)
  pure $
    Enemy
      { eHp = hp,
        eAtk = atk,
        eName = "Enemy"
      }
