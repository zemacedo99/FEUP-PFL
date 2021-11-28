-- |
-- Module      : Utils
-- Description : Funções de utilidade
-- Copyright   : (c) PFL G6_07, 2021
-- License     : GPL-3
-- Stability   : experimental
-- Portability : POSIX.
module Utils where

truncateString :: String ->  String
truncateString str 
  | str == "0" = "0"
  | otherwise = if isNegativeNumber then '-' : truncated else truncated
  where
    isNegativeNumber = head str == '-'
    truncated = dropWhile (== '0') (if isNegativeNumber then tail str else str)

xor :: Bool -> Bool -> Bool
xor x y | x && not y = True
        | not x && y = True
        | otherwise = False