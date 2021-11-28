
-- |
-- Module      : BigNumber
-- Description : Implementação de uma biblioteca de big-numbers em Haskell
--               para representar estruturas para big-numbers
-- Copyright   : (c) PFL G6_07, 2021
-- License     : GPL-3
-- Stability   : experimental
-- Portability : POSIX.
module BigNumber where
  
import Utils ( truncateString, xor )

-- *** 2 ***

-- 2.1
-- BigNumber type definition
type BigNumber = [Int]

-- 2.2
-- | scanner: converts String into BN
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
-- | output: converts BN into String
output :: BigNumber -> String
output bn
  | isNegBN bn = "-" ++ show (head bn * (-1)) ++ [head (show digit) | digit <- drop 1 bn]
  | otherwise = [head (show digit) | digit <- bn]

-- Auxiliar Functions for BigNumbers

-- | minBN: returns the smaller BN between two.
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
      | otherwise = head bn1 : maxBN (drop 1 bn1) (drop 1 bn2)
    minPos
      | output bn1 == output bn2 = bn1
      | length bn1 < length bn2 = bn1
      | length bn2 < length bn1 = bn2
      | head bn1 < head bn2 = bn1
      | head bn2 < head bn1 = bn2
      | otherwise = head bn1 : minBN (drop 1 bn1) (drop 1 bn2)

-- | maxBN: returns the bigger BN between two.
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
      | otherwise = head bn1 : minBN (drop 1 bn1) (drop 1 bn2)
    maxPos
      | output bn1 == output bn2 = bn1
      | length bn1 > length bn2 = bn1
      | length bn2 > length bn1 = bn2
      | head bn1 > head bn2 = bn1
      | head bn2 > head bn1 = bn2
      | otherwise = head bn1 : maxBN (drop 1 bn1) (drop 1 bn2)

-- | negBN: returns the BN with opposite parity.
negBN :: BigNumber -> BigNumber
negBN bn = (head bn * (-1)) : tail bn

-- | isZeroBN: checks if a BN is zero. If it is, returns True.
isZeroBN :: BigNumber -> Bool
isZeroBN bn = sum bn == 0

-- | isNegBN: checks if a BN is negative. If it is, returns True.
isNegBN :: BigNumber -> Bool
isNegBN bn = head bn < 0

-- | isPosBN: checks if a BN is positive. If it is, returns True.
isPosBN :: BigNumber -> Bool
isPosBN bn = head bn > 0

-- | absBN: returns the absolute value of the BN.
absBN :: BigNumber -> BigNumber
absBN bn
 | isNegBN bn = (head bn * (-1)) : drop 1 bn
 | otherwise = bn

-- | gtBN: checks if the first BN is bigger than the second BN. If it is, returns True.
gtBN :: BigNumber -> BigNumber -> Bool
gtBN bn1 bn2
  | bn1 `equalsBN` bn2 = False
  | otherwise = maxBN bn1 bn2 == bn1

-- | ltBN: checks if the first BN is lesser than the second BN. If it is, returns True.
ltBN :: BigNumber -> BigNumber -> Bool
ltBN bn1 bn2
  | bn1 `equalsBN` bn2 = False
  | otherwise = maxBN bn1 bn2 == bn2

-- | gtEqualBN: checks if the first BN is bigger than or equal to the second BN. If it is, 
--              returns True.
gtEqualBN :: BigNumber -> BigNumber -> Bool
gtEqualBN bn1 bn2
  | bn1 `equalsBN` bn2 = True
  | otherwise = maxBN bn1 bn2 == bn1
  
-- | ltEqualBN: checks if the first BN is lesser than or equal to the second BN. If it is, 
--              returns True.
ltEqualBN :: BigNumber -> BigNumber -> Bool
ltEqualBN bn1 bn2
  | bn1 `equalsBN` bn2 = True
  | otherwise = maxBN bn1 bn2 == bn2

-- | equalsBN: checks if the first BN is equal to the second BN. If it is, returns True.
equalsBN :: BigNumber -> BigNumber -> Bool
equalsBN bn1 bn2 = bn1 == bn2

-- 2.4
-- | somaBN': sums 2 BNs recursively using carry
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

-- | somaBN: checks base cases for sum operation and calls the appropriate functions to execute 
--           the operation (either somaBN' or subBN) 
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN bn1 bn2
  | isNegBn1 && isNegBn2 = negBN (absBn1 `somaBN` absBn2)
  | isNegBn1 && (absBn1 `gtBN` absBn2) = negBN (absBn1 `subBN` absBn2)
  | isNegBn1 && (absBn1 `ltBN` absBn2) = bn2 `subBN` absBn1
  | isNegBn2 && not (absBn1 `equalsBN` absBn2) = bn1 `subBN` absBn2
  | otherwise = reverse (somaBN' (reverse bn1) (reverse bn2) 0)
  where
    absBn2 = absBN bn2
    absBn1 = absBN bn1
    isNegBn1 = isNegBN bn1
    isNegBn2 = isNegBN bn2

-- 2.5
-- | subBN': subtracts 2 BNs recursively using carry
subBN' :: [Int] -> [Int] -> Int -> [Int]
subBN' [] x carry
  | carry == 0 = x
  | otherwise = subBN' x [carry] 0

subBN' x [] carry
  | carry == 0 = x
  | otherwise = subBN' x [carry] 0

subBN' (x : xs) (y : ys) carry =  val : subBN' xs ys res
  where
    ny
      | y + carry >= 10 = 0
      | otherwise = y + carry
    nc
      | y + carry >= 10 = 1
      | otherwise = 0
    val
      | x >= ny =  x - ny
      | otherwise = x + 10 - ny
    res = if x >= ny then nc else 1 + nc

-- | subBN': checks base cases for subtraction operation and calls the appropriate functions to 
--           execute the operation (either somaBN or subBN') 
subBN :: BigNumber -> BigNumber -> BigNumber
subBN bn1 bn2
  | isNegBn1 && isNegBn2 = bn1 `somaBN` absBn2
  | isNegBn1 && ((absBn1 `gtBN` absBn2) || (absBn1 `ltBN` absBn2)) = negBN (absBn1 `somaBN` absBn2)
  | isNegBn2 && absBn2 `gtBN` absBn1 = absBn1 `somaBN` absBn2
  | isNegBn2 && absBn2 `ltBN` absBn1 = bn1 `somaBN` absBn2
  | absBn1 `equalsBN` absBn2 = [0]
  | bn1 `gtBN` bn2 = scanner(output(reverse (subBN' revBn1 revBn2 0)))
  | otherwise = negBN (scanner(output(reverse (subBN' revBn2 revBn1 0))))
  where
    revBn1 = reverse bn1
    revBn2 = reverse bn2
    absBn1 = absBN bn1
    absBn2 = absBN bn2
    isNegBn1 = isNegBN bn1
    isNegBn2 = isNegBN bn2

-- 2.6
-- | mulBN'': multiplies 2 BNs recursively using carry 
mulBN'' :: Int -> Int -> BigNumber -> BigNumber
mulBN'' a carry bn
  | null bn && carry == 0 = bn
  | null bn = [carry]
  | a == 0 = [0 | i <- bn]
  | a == 1 = bn
  | otherwise = res : mulBN'' a nextCarry (drop 1 bn)
  where
    res = if op >= 10 then op `rem` 10 else op
    nextCarry = if op >= 10 then op `div` 10 else 0
    op = a * head bn + carry

-- | mulBN': multiplies 2 BNs using comprehension lists 
mulBN' :: BigNumber -> BigNumber -> BigNumber
mulBN' bn1 bn2 = foldl somaBN [0] resMulZeros
  where
    rbn1 = reverse bn1
    rbn2 = reverse bn2
    resMulZeros = head resMul : [resMul !! i ++ replicate i 0 | i <- [0..length resMul - 1], i > 0]
    resMul = [reverse (mulBN'' a 0 rbn1) | a <- rbn2]

-- | mulBN: checks base cases for multiplication operation and calls then calls mulBN'
mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN bn1 bn2
  | output bn1 == "-1" = negBN bn2
  | output bn2 == "-1" = negBN bn1
  | isZeroBN bn1 || isZeroBN bn2 = [0]
  | isNegBN bn1 `xor` isNegBN bn2 = negBN (mulBN' (negBN negative) positive)
  | isNegBN bn1 && isNegBN bn2 =  mulBN' (negBN bn1)  (negBN bn2)
  | otherwise = mulBN' bn1 bn2
  where
    negative = if isNegBN bn1 then bn1 else bn2
    positive = if isNegBN bn1 then bn2 else bn1

-- 2.7
-- divBN: divides 2 BNs using comprehension list with multiples of the divisor. Uses the function 
--        takeWhile and ltEqualBN to stop the list generation. 
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN bn1 bn2 
  | bn1 `gtEqualBN` bn2 = (quociente,resto)
  | bn1 `ltBN` bn2 = ([0], bn1)
  | otherwise = ([0],[0])
  where
    multiplesListInf = [mulBN bn x | bn <- repeat bn2, x <- listaInfBN [1]]
    multiplesList = takeWhile (`ltEqualBN` bn1 ) multiplesListInf
    lastMultiple = last multiplesList
    quociente = scanner (show (length multiplesList))
    resto     = scanner (output (bn1 `subBN` lastMultiple))

listaInfBN :: BigNumber -> [BigNumber]
listaInfBN n = n : listaInfBN (n `somaBN` [1])

-- 5
-- | safeDivBN: ensures that divBN can divide a BN with zero, using Monad of Maybe type. 
--              When bn2 is zero, it returns Nothing. Otherwise, it calls divBN.
safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN bn1 bn2
  | isZeroBN bn2 = Nothing 
  | otherwise = Just(divBN bn1 bn2)