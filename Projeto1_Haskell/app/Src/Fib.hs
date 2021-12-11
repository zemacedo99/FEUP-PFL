-- |
-- Module      : Fib
-- Description : Implementação de uma biblioteca de big-numbers em Haskell, incluindo operações
--               aritméticas básicas, para cálculo da sequência de Fibonacci.
-- Copyright   : (c) PFL G6_07, 2021
-- License     : GPL-3
-- Stability   : experimental
-- Portability : POSIX.
module Fib where

import BigNumber
    ( equalsBN, listaInfBN, output, scanner, somaBN, subBN, BigNumber )

-- *** 1 ***

-- 1.1
-- | fibRec: normal recursion algorythm for Fibonacci. Two base cases and two recursion calls.
fibRec :: (Integral a) => a -> a
fibRec n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fibRec (n - 1) + fibRec (n - 2)

-- 1.2
-- | fibLista2: In this version of FibLista, we use the function **foldl** so we can access the 
--               partial results of the list without needing to wait for the whole processing.
--               It also ensures compiler optimization without bursting the stack.
fibLista2' :: Num b => (b, b) -> p -> (b, b)
fibLista2' (a, b) _ = (b, a + b)

fibLista2 :: Num b => Int -> b
fibLista2 n = snd (foldl fibLista2' (1, 1) (replicate (n - 1) 0))

-- | fibLista: Calculates the nth Fibonacci number using the higher order function **map** to 
--             create the Fibonacci list and the index operator **!!** to access the partial result, which is the nth number.
fibLista :: Num a => Int -> a
fibLista n = fibLista !! n
  where
    fibLista = 1 : 1 : map f [2 ..]
    f n = fibLista !! (n - 1) + fibLista !! (n - 2)

-- 1.3
-- | fibListaInfinita:  Calculates an infinite list of Fibonnacci numbers by producing the list of 
--                      corresponding sums of the ever growing Fibonnacci list with its tail. 
--                      Uses the function **zipWith** to do that.
fibListaInfinita :: Num a => Int -> a
fibListaInfinita n =
  let fib = 1 : 1 : zipWith (+) fib (tail fib)
   in fib !! n

-- *** 3 ***

-- 3.1
-- | fibRecBN: normal fibonnacci recursion using BigNumber type
fibRecBN' :: BigNumber -> BigNumber
fibRecBN' bn
  | bn `equalsBN` [0] = [1]
  | bn `equalsBN` [1] = [1]
  | otherwise = fibRecBN' (bn `subBN` [1]) `somaBN` fibRecBN' (bn `subBN` [2])

fibRecBN :: String -> String
fibRecBN n = output (fibRecBN' (scanner n))

-- 3.2
-- | fibListaBN: Optimized version of the recussive fibonnaci using a memoized list of results 
--               and lazy evaluation
fibListaBN :: Int -> String
fibListaBN n = output (lista !! n)
  where
    lista = [1] : [1] : map fib (listaInfBN [2])
    fib n = (lista !! read (output (n `subBN` [2])))`somaBN` (lista !! read (output (n `subBN` [1])))

-- 3.3
-- | fibListaInfinitaBN: generalises zip by zipping with the function given as the first argument, 
--                       instead of a tupling function to create an infinite list of BigNumber fibonnacci sequence
fibListaInfinitaBN' :: Int -> BigNumber
fibListaInfinitaBN' n =
  let fib = [1] : [1] : zipWith somaBN fib (tail fib)
   in fib !! n

fibListaInfinitaBN :: Int -> String
fibListaInfinitaBN n = output (fibListaInfinitaBN' n)