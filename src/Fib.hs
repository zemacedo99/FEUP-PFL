-- |
-- Module      : Fib
-- Description : Implementação de uma biblioteca de big-numbers em Haskell, incluindo operações
--               aritméticas básicas, para cálculo da sequência de Fibonacci.
-- Copyright   : (c) PFL G6_07, 2021
-- License     : GPL-3
-- Stability   : experimental
-- Portability : POSIX.
module Fib where

import BigNumber ()

-- 1.1
-- Normal recursion algorythm for fibonacci
fibRec :: (Integral a) => a -> a
fibRec n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fibRec (n - 1) + fibRec (n - 2)

-- 1.2
-- Using fold to write it can ensure that the compiler optimizes the recursion without bursting the stack.
-- At the same time, in my opinion, the fold process reflects the change of state. The initial state is
-- calculated step by step to get the final state, which is in line with the idea of ​​dynamic programming,
-- so I think this is a recursive form of dynamic programming in functional programming.

fibLista' :: Num b => (b, b) -> p -> (b, b)
fibLista' (a, b) _ = (b, a + b)

fibLista :: Num b => Int -> b
fibLista n = snd (foldl fibLista' (0, 1) (replicate (n - 1) 0))

fibLista2 :: Num a => Int -> a
fibLista2 n = fibLista2 !! n
  where
    fibLista2 = 0 : 1 : map f [2 ..]
    f n = fibLista2 !! (n -1) + fibLista2 !! (n -2)

-- 1.3
-- Calculates an infinite list of fibonnacci numbers by producing the list of corresponding sums
-- of the ever growing fibonnacci list with its tail

fibListaInfinita :: Num a => Int -> a
fibListaInfinita n =
  let fib = 0 : 1 : zipWith (+) fib (tail fib)
   in fib !! n