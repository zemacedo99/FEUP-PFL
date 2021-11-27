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
minBN bn1 bn2 = min
  where
    min
      | isNegBN bn1 && isNegBN bn2  =  minNeg
      | isNegBN bn1  =  bn1
      | isNegBN bn2  =  bn2
      | otherwise = minPos
    minNeg
      | output bn1 == output bn2 = bn1
      | length bn1 > length bn2 = bn1
      | length bn2 > length bn1 = bn2
      | head bn1 > head bn2 = bn1
      | head bn2 > head bn1 = bn2
      | otherwise = maxBN (drop 1 bn1) (drop 1 bn2)
    minPos
      | output bn1 == output bn2 = bn1
      | length bn1 < length bn2 = bn1
      | length bn2 < length bn1 = bn2
      | head bn1 < head bn2 = bn1
      | head bn2 < head bn1 = bn2
      | otherwise = minBN (drop 1 bn1) (drop 1 bn2)

maxBN :: BigNumber -> BigNumber -> BigNumber
maxBN bn1 bn2 = max
  where
    max
      | isNegBN bn1 && isNegBN bn2  =  maxNeg
      | isNegBN bn1  =  bn2
      | isNegBN bn2  =  bn1
      | otherwise = maxPos
    maxNeg
      | output bn1 == output bn2 = bn1
      | length bn1 > length bn2 = bn2
      | length bn2 > length bn1 = bn1
      | head bn1 > head bn2 = bn2
      | head bn2 > head bn1 = bn1
      | otherwise = minBN (drop 1 bn1) (drop 1 bn2)
    maxPos
      | output bn1 == output bn2 = bn1
      | length bn1 > length bn2 = bn1
      | length bn2 > length bn1 = bn2
      | head bn1 > head bn2 = bn1
      | head bn2 > head bn1 = bn2
      | otherwise = maxBN (drop 1 bn1) (drop 1 bn2)

negBN :: BigNumber -> BigNumber
negBN bn = (head bn * (-1)) : tail bn

isZeroBN :: BigNumber -> Bool
isZeroBN bn = sum bn == 0

isNegBN :: BigNumber -> Bool
isNegBN bn = head bn < 0

isPosBN :: BigNumber -> Bool
isPosBN bn = head bn > 0

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
    | head bigger >= head smallerWithZeros = (head bigger - head smallerWithZeros) : subBN' ( drop 1 smallerWithZeros) ( drop 1 bigger)
    | otherwise = (10 + head bigger - head smallerWithZeros) : subBN' biggerWithCarry (drop 1 smaller)
  where
    bigger =  maxBN bn1 bn2
    smaller =  minBN bn1 bn2
    smallerWithZeros =  smaller ++ take subLen zeros
    subLen = length bigger - length smaller
    biggerWithCarry = ((drop 1 bigger !! 1) - 1) : drop 2 bigger
    zeros = [0 | n <- [1..9999]]

subBN :: BigNumber -> BigNumber -> BigNumber
subBN bn1 bn2
  | isZeroBN bn1 = negBN bn2
  | isZeroBN bn2 = bn1
  | maxBN bn1 bn2 == bn1 = res
  | otherwise = negBN res
  where
    res = reverse (subBN' (reverse bn1) (reverse bn2))



subBN2' :: [Int] -> [Int] -> Int -> [Int]
subBN2' [] x carry
  | carry == 0 = x
  | otherwise = []
subBN2' x [] carry
  | carry == 0 = x
  | otherwise = []

--head xs - 1: tail xs
subBN2' (x : xs) (y : ys) carry  = val : subBN2' xs ys res
  where
    ny
      | y + carry >= 10 = 0
      | otherwise = y + carry
    nc
      | y + carry >= 10 = 1
      | otherwise = 0
    val
      | x >= ny =  x - ny 
      | x < ny = (x + 10) - ny
    res = if x >= ny then nc  else  1 + nc

subBN2 :: BigNumber -> BigNumber -> BigNumber
subBN2 bn1 bn2
  | maxBN bn1 bn2 == bn1 = reverse (subBN2' (reverse bn1) (reverse bn2) 0)
  | otherwise = negBN(reverse (subBN2' (reverse bn2) (reverse bn1) 0))


-- 2.6
-- mulBN: multiply 2 big numbers
mulBN'' :: Int -> Int -> BigNumber -> BigNumber
mulBN'' a carry bn
  | null bn = bn
  | a == 0 = [0 | i <- bn]
  | a == 1 = bn
  | otherwise = res : mulBN'' a nextCarry (drop 1 bn)
  where
    res = if op >= 10 then op `rem` 10 else op
    nextCarry = if op >= 10 then op `div` 10 else 0
    op = (a * head bn) + carry

mulBN' :: BigNumber -> BigNumber -> BigNumber
mulBN' bn1 bn2 = foldl somaBN [0] resMulZeros
  where
    rbn1 = reverse bn1
    rbn2 = reverse bn2
    resMulZeros = head resMul : [resMul !! i ++ replicate i 0 | i <- [0..length resMul - 1], i > 0]
    resMul = [reverse (mulBN'' a 0 rbn1) | a <- rbn2]

mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN bn1 bn2
  | output bn1 == "-1" = head bn2 * (-1) : drop 1 bn2
  | output bn2 == "-1" = head bn1 * (-1) : drop 1 bn1
  | isZeroBN bn1 || isZeroBN bn2 = [0]
  | isNegBN bn1 `xor` isNegBN bn2 = negBN (mulBN' (negBN negative) positive)
  | otherwise = mulBN' bn1 bn2
  where
    negative = if isNegBN bn1 then bn1 else bn2
    positive = if isNegBN bn1 then bn2 else bn1

-- 2.7
-- divBN: divide 2 big numbers
-- divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)

divBN :: BigNumber -> BigNumber -> [BigNumber]
divBN bn1 bn2= [mulBN bn x| bn <- repeat bn1, x <- listaInfBN 1,  maxBN (mulBN bn x) bn2 == mulBN bn x]


listaInfBN :: Int -> [BigNumber]
listaInfBN n = [n] : listaInfBN (n + 1)


-- divBN []  bn2 = ([],[])
-- divBN bn1 [] = ([],[])
-- divBN bn1 bn2
--   | maxBN bn1 bn2 == bn1 = (quo,res)
--   | otherwise = ([],bn1)

--   where
--         newDivisor = if res /= [0] then head res : drop 1 divisor else drop 1 divisor
--         quo =  (head divisor `div` head bn2) : fst (divBN newDivisor bn2)
--         res =  [head divisor `mod` head bn2]
--         divisor = if head bn1 < head bn2
--                   then (head bn1 * 10 + head (tail bn1)) : drop 1 (tail bn1)
--                   else bn1

-- todo: divBN [9,2,1] [2] = [4,6] errado e  divBN [9,2,9] [2] = [4,6,4] certo why? o mesmo acontece para quando 20 / 2
-- todo: divBN divBN [9,2,2] [2] da resto 1 e devia dar resto 0
-- todo: divisor fazer o then recursivo até divisor >= head bn2 
-- todo: fazer um dividendo como o divisor para nao usar head bn2 s 

