{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use map with tuple-section" #-}
module Game where

import Prelude
import Graphics.Gloss
import Data.Array

data Player = PlayerX | PlayerO deriving (Eq, Show)
data Cell = Empty |  Full Player deriving (Show, Eq)
data State = Running | GameOver (Maybe Player) deriving (Show, Eq)

type Board = Array (Int, Int) Cell

data Game = Game {
  gameBoard :: Board,
  gamePlayer :: Player,
  gameState :: State
} deriving (Eq, Show)

n :: Int
n = 3

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame :: Game
initialGame = Game { 
  gameBoard = array indexRange $ zip (range indexRange) (cycle [Empty]), 
  gamePlayer = PlayerO, 
  gameState = Running
} 
  where indexRange = ((0, 0),(n - 1 , n - 1))

