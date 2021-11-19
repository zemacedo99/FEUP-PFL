-- |
-- Module      : Utils
-- Description : Funções de utilidade
-- Copyright   : (c) PFL G6_07, 2021
-- License     : GPL-3
-- Stability   : experimental
-- Portability : POSIX.
module Utils where

truncateString :: String ->  String
truncateString str = if isNegativeNumber then '-' : truncated else truncated
  where
    isNegativeNumber = head str == '-'
    truncated = dropWhile (== '0') (if isNegativeNumber then tail str else str)  