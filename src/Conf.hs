{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Wolfram
-}

module Conf (Conf(rule, start, line, window, move), defaultConf) where

data Conf = Conf {
  rule   :: Int,
  start  :: Int,
  line   :: Int,
  window :: Int,
  move   :: Int
} deriving (Show)

defaultConf :: Conf
defaultConf = Conf {
  rule   = -1,
  start  = 0,
  line   = -1,
  window = 80,
  move   = 0
}
