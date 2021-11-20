-- |
-- Module      : BigNumber
-- Description : Implementação de uma biblioteca de big-numbers em Haskell
--               para representar estruturas para big-numbers
-- Copyright   : (c) PFL G6_07, 2021
-- License     : GPL-3
-- Stability   : experimental
-- Portability : POSIX.
module BigNumber where

import Utils
import Text.Html (sub)

-- *** 2 ***

-- 2.1
-- BigNumber type definition
type BigNumber = [Int]

-- 2.2
-- scanner: convert string into big number
scanner :: String -> BigNumber
scanner str
  | isNegativeNumber = head bigNumber * (-1) : tail bigNumber
  | otherwise = bigNumber
  where
    truncatedStr = truncateString str
    isNegativeNumber = head str == '-'
    numberStr = if isNegativeNumber then tail truncatedStr else truncatedStr
    bigNumber = map (read . (: "")) numberStr :: BigNumber

-- 2.3
-- output: convert big number into string
output :: BigNumber -> String
output bn
  | isNegBN bn = "-" ++ show (head bn * (-1)) ++ [head (show digit) | digit <- drop 1 bn]
  | otherwise = [head (show digit) | digit <- bn]

-- AUX
minBN :: BigNumber -> BigNumber -> BigNumber
minBN bn1 bn2 = minimum
  where
    minimum
      | output bn1 == output bn2 = bn1
      | length bn1 < length bn2 = bn1
      | length bn2 < length bn1 = bn2
      | head bn1 < head bn2 = bn1
      | head bn2 < head bn1 = bn2
      | otherwise = minBN (drop 1 bn1) (drop 1 bn2)

maxBN :: BigNumber -> BigNumber -> BigNumber
maxBN bn1 bn2 = minimum
  where
    minimum
      | output bn1 == output bn2 = bn1
      | length bn1 > length bn2 = bn1
      | length bn2 > length bn1 = bn2
      | head bn1 > head bn2 = bn1
      | head bn2 > head bn1 = bn2
      | otherwise = minBN (drop 1 bn1) (drop 1 bn2)

negBN :: BigNumber -> BigNumber
negBN bn = (head bn * (-1)) : tail bn

isZeroBN :: BigNumber -> Bool
isZeroBN bn = sum bn == 0

isNegBN :: BigNumber -> Bool
isNegBN bn = head bn < 0

-- 2.4
-- somaBN: sum 2 big numbers
somaBN' :: Integral t => [t] -> [t] -> t -> [t]
somaBN' [] x carry
  | carry == 0 = x
  | otherwise = somaBN' [carry] x 0
somaBN' x [] carry
  | carry == 0 = x
  | otherwise = somaBN' [carry] x 0
somaBN' (x : xs) (y : ys) carry = val : somaBN' xs ys res
  where
    val = (x + y + carry) `rem` 10
    res = (x + y + carry) `quot` 10

somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN bn1 bn2
  | isNegBN bn1 || isNegBN bn2 = subBN bn1 bn2
  | otherwise = reverse (somaBN' (reverse bn1) (reverse bn2) 0)

-- 2.5
-- subBN: subtract 2 big numbers
subBN' :: BigNumber -> BigNumber -> BigNumber
subBN' bn1 bn2
    | null bn1 = bn2
    | null bn2 = bn1
    | head bigger >= head smallerWithZeros = (head bigger - head smallerWithZeros) : subBN' (reverse (drop 1 smallerWithZeros)) (reverse (drop 1 bigger))
    | otherwise = (10 + head bigger - head smallerWithZeros) : subBN' biggerWithCarry (drop 1 (reverse smaller))
  where
    bigger = reverse (maxBN bn1 bn2)
    smaller = reverse (minBN bn1 bn2)
    smallerWithZeros =  smaller ++ take subLen zeros
    subLen = length bigger - length smaller
    biggerWithCarry = (drop 1 bigger !! 1 + 1) : drop 2 bigger
    zeros = [0 | n <- [1..9999]]

subBN :: BigNumber -> BigNumber -> BigNumber
subBN bn1 bn2
  | isZeroBN bn1 = negBN bn2
  | isZeroBN bn2 = bn1
  | max bn1 bn2 == bn1 = res
  | otherwise = negBN res
  where
    res = reverse (subBN' bn1 bn2)

-- 2.6
-- mulBN: multiply 2 big numbers
mulBN' :: BigNumber -> BigNumber -> BigNumber
mulBN' bn1 bn2 = rbn1
  where
    rbn1 = reverse bn1
    rbn2 = reverse bn2

mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN bn1 bn2
  | isZeroBN bn1 || isZeroBN bn2 = scanner "0"
  | otherwise = reverse (mulBN' bn1 bn2)

-- 2.7
-- divBN: divide 2 big numbers
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN x y = (scanner "123", scanner "321")