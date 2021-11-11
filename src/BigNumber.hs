-- |
-- Module      : BigNumber
-- Description : Implementação de uma biblioteca de big-numbers em Haskell
--               para representar estruturas para big-numbers
-- Copyright   : (c) PFL G6_07, 2021
-- License     : GPL-3
-- Stability   : experimental
-- Portability : POSIX.
module BigNumber where

import Data.List.Split
import Data.Text.Internal.Read (digitToInt)

type BigNumber = [Int]

scanner :: String -> BigNumber
scanner str = bigNumber
  where
    splitString = splitOn "" str
    bigNumber = [digitToInt (splitString !! n) | n <- [0 .. (length (splitString - 1))]]