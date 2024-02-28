{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Wolfram
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Main (main) where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isJust )
import Conf (Conf(rule, start, line, window, move), defaultConf)

-- PARSING

getRule :: String -> Int
getRule s = fromJust (readMaybe s :: Maybe Int)

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] | rule conf < 0         = Nothing
                | rule conf > 255       = Nothing
                | start conf < 0        = Nothing
                | line conf < -1        = Nothing
                | window conf < 0       = Nothing
                | otherwise             = Just conf
getOpts conf ("--rule":y:ys)
  | isJust (readMaybe y :: Maybe Int) = getOpts (conf {rule = getRule y}) ys
  | otherwise                         = Nothing
getOpts conf ("--start":y:ys)
  | isJust (readMaybe y :: Maybe Int) = getOpts (conf {start = getRule y}) ys
  | otherwise                         = Nothing
getOpts conf ("--lines":y:ys)
  | isJust (readMaybe y :: Maybe Int) = getOpts (conf {line = getRule y}) ys
  | otherwise                         = Nothing
getOpts conf ("--window":y:ys)
  | isJust (readMaybe y :: Maybe Int) = getOpts (conf {window = getRule y}) ys
  | otherwise                         = Nothing
getOpts conf ("--move":y:ys)
  | isJust (readMaybe y :: Maybe Int) = getOpts (conf {move = getRule y}) ys
  | otherwise                         = Nothing
getOpts _ _                           = Nothing

--

ruleToBools :: Int -> [Bool]
ruleToBools n = ruleToBools' n 0

ruleToBools' :: Int -> Int -> [Bool]
ruleToBools' n i
  | i > 7     = []
  | otherwise = ((n `div` (2^i)) `mod` 2 == 1) : ruleToBools' n (i+1)

getFLine :: [Bool]
getFLine = replicate 1 False ++ [True] ++ replicate 1 False

boolToChar :: Bool -> Char
boolToChar True = '*'
boolToChar False = ' '

getPaddingRight :: Int -> [Char] -> [Char]
getPaddingRight offset ys | offset < 0 = ys ++ replicate (-offset) ' '
                          | otherwise  = take (length ys - offset) ys

printLine :: Int -> Int -> [Bool] -> IO ()
printLine offset y xs = printLine' offset y paddingLeft line' paddingRight
  where start' = (length xs - y) `div` 2
        line' | (start' - offset < 0) && (offset /= 0) =
            replicate (offset - start') ' ' ++ take y (map boolToChar xs)
              | otherwise = take y (drop (start' - offset) (map boolToChar xs))
        padding = replicate ((y - length line') `div` 2) ' '
        paddingRight = getPaddingRight offset padding
        len = length padding + length line' + length paddingRight
        paddingLeft | len < y = padding ++ " "
                       | otherwise = padding

printLine' :: Int -> Int -> [Char] -> [Char] -> [Char] -> IO ()
printLine' x y pLeft line' pRight | x < 0 = putStrLn (drop (length s - y) s)
                                  | otherwise  = putStrLn (take y s)
  where s = pLeft ++ line' ++ pRight


generateLines :: Conf -> IO ()
generateLines conf =
  gen (move conf) (start conf) (line conf) (window conf) getFLine (rule conf)

gen :: Int -> Int -> Int -> Int -> [Bool] -> Int -> IO ()
gen _ _ 0 _ _ _ = return ()
gen x y len win line' rule' 
  | y > 0 = gen x (y - 1) len win next rule'
  | otherwise = printLine x win line' >> gen x y (len - 1) win next rule'
  where next = [False] ++ applyRule rule' line' ++ [False]

applyRule :: Int -> [Bool] -> [Bool]
applyRule _ [] = []
applyRule rule' xs = applyRule' rule' (withNeighbors xs)

applyRule' :: Int -> [(Bool, Bool, Bool)] -> [Bool]
applyRule' _ [] = []
applyRule' rule' (x:xs) = applyRuleToCell rule' x : applyRule' rule' xs

withNeighbors :: [Bool] -> [(Bool, Bool, Bool)]
withNeighbors xs = zip3 (False:xs) xs (tail xs ++ [False])

applyRuleToCell :: Int -> (Bool, Bool, Bool) -> Bool
applyRuleToCell rule' (True, True, True)    = ruleToBools rule' !! 7
applyRuleToCell rule' (True, True, False)   = ruleToBools rule' !! 6
applyRuleToCell rule' (True, False, True)   = ruleToBools rule' !! 5
applyRuleToCell rule' (True, False, False)  = ruleToBools rule' !! 4
applyRuleToCell rule' (False, True, True)   = ruleToBools rule' !! 3
applyRuleToCell rule' (False, True, False)  = ruleToBools rule' !! 2
applyRuleToCell rule' (False, False, True)  = ruleToBools rule' !! 1
applyRuleToCell rule' (False, False, False) = head (ruleToBools rule')

main :: IO ()
main = do
  args <- getArgs
  case getOpts defaultConf args of
    Just conf -> generateLines conf
    Nothing -> putStrLn "ERROR" >> exitWith (ExitFailure 84)
