-- |
-- Module      : BigNumber
-- Description : Implementação de uma biblioteca de big-numbers em Haskell
--               para representar estruturas para big-numbers
-- Copyright   : (c) PFL G6_07, 2021
-- License     : GPL-3
-- Stability   : experimental
-- Portability : POSIX.
module BigNumber where

type BigNumber = [Int]


-- applying (:"") to the string we convert each Char of the string (string is just a list of Chars) into a single-element list.
-- next the function read convert the individual strings (Char) into integers.
-- the map applys that to every char of the string, producing our BigNumber (just a list of Ints)

scanner :: String -> BigNumber
scanner str = map (read . (:"")) str :: BigNumber

output :: BigNumber -> String
output bn = [head (show str) | str <- bn]
  