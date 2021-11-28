# Programação Funcional e Lógica

![feuplogo](feup_logo.jpg)

#### Contributors

**G6_07**

- [Deborah Lago](mailto:@up.pt) up201806102
- [José Macedo](mailto:@up.pt) up201705226
- [Tiago Lima Rocha](mailto:up201406679@up.pt) up201406679

## Table of Contents

- [Programação Funcional e Lógica](#programação-funcional-e-lógica)
      - [Contributors](#contributors)
  - [Table of Contents](#table-of-contents)
  - [Abstract](#abstract)
  - [Functions and Test Cases](#functions-and-test-cases)
    - [BigNumber Module](#bignumber-module)
      - [scanner :: String -> BigNumber](#scanner--string---bignumber)
      - [output :: BigNumber -> String](#output--bignumber---string)
      - [somaBN :: BigNumber -> BigNumber -> BigNumber](#somabn--bignumber---bignumber---bignumber)
      - [subBN :: BigNumber -> BigNumber -> BigNumber](#subbn--bignumber---bignumber---bignumber)
      - [mulBN :: BigNumber -> BigNumber -> BigNumber](#mulbn--bignumber---bignumber---bignumber)
      - [divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)](#divbn--bignumber---bignumber---bignumber-bignumber)
      - [safeDivBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)](#safedivbn--bignumber---bignumber---bignumber-bignumber)
    - [Fib Module](#fib-module)
      - [fibRec :: (Integral a) => a -> a](#fibrec--integral-a--a---a)
      - [fibLista2 :: Num b => Int -> b](#fiblista2--num-b--int---b)
      - [fibLista :: Num a => Int -> a](#fiblista--num-a--int---a)
      - [fibListaInfinita :: Num a => Int -> a](#fiblistainfinita--num-a--int---a)
      - [fibRecBN :: String -> String](#fibrecbn--string---string)
      - [fibListaBN :: String -> String](#fiblistabn--string---string)
      - [fibListaInfinitaBN :: Int -> String](#fiblistainfinitabn--int---string)
    - [Utils Module](#utils-module)
      - [truncateString :: String ->  String](#truncatestring--string----string)
      - [xor :: Bool -> Bool -> Bool](#xor--bool---bool---bool)
  - [Conclusion *(Exercice 4)*](#conclusion-exercice-4)
  - [Auxiliar Functions to BigNumber Module](#auxiliar-functions-to-bignumber-module)
      - [minBN :: BigNumber -> BigNumber -> BigNumber](#minbn--bignumber---bignumber---bignumber)
      - [maxBN :: BigNumber -> BigNumber -> BigNumber](#maxbn--bignumber---bignumber---bignumber)
      - [negBN :: BigNumber -> BigNumber](#negbn--bignumber---bignumber)
      - [isZeroBN :: BigNumber -> Bool](#iszerobn--bignumber---bool)
      - [isNegBN :: BigNumber -> Bool](#isnegbn--bignumber---bool)
      - [isPosBN :: BigNumber -> Bool](#isposbn--bignumber---bool)
      - [absBN :: BigNumber -> BigNumber](#absbn--bignumber---bignumber)
      - [gtBN :: BigNumber -> BigNumber -> Bool](#gtbn--bignumber---bignumber---bool)
      - [ltBN :: BigNumber -> BigNumber -> Bool](#ltbn--bignumber---bignumber---bool)
      - [ltEqualBN :: BigNumber -> BigNumber -> Bool](#ltequalbn--bignumber---bignumber---bool)
      - [gtEqualBN :: BigNumber -> BigNumber -> Bool](#gtequalbn--bignumber---bignumber---bool)
      - [equalsBN :: BigNumber -> BigNumber -> Bool](#equalsbn--bignumber---bignumber---bool)

## Abstract
  
## Functions and Test Cases

> uma explicação sucinta do funcionamento de cada função;
> a descrição de vários casos de teste para todas as funções;

### BigNumber Module
  [BigNumber Module]()
  > Source Code: BigNumber.hs
  [BigNumber Tests]()
  > Tests: BigNumber.test.hs

#### scanner :: String -> BigNumber

  > converts String into BN. Uses recursion and higher order function **map** to apply **read** to every Char of the String, prooducing the BigNumber(BN).

```haskell
scanner :: String -> BigNumber
scanner str
  | isNegativeNumber = head bigNumber * (-1) : tail bigNumber
  | otherwise = bigNumber
  where
    truncatedStr = truncateString str
    isNegativeNumber = head str == '-'
    numberStr = if isNegativeNumber then tail truncatedStr else truncatedStr
    bigNumber = map (read . (: "")) numberStr :: BigNumber
```

  > Tests
```
scanner1 = TestCase (assertEqual "for (scanner '123')," [1,2,3] (scanner "123"))
scanner2 = TestCase (assertEqual "for (scanner '-123')," [-1,2,3] (scanner "-123"))
scanner3 = TestCase (assertEqual "for (scanner '-000123')," [-1,2,3] (scanner "-000123"))
scanner4 = TestCase (assertEqual "for (scanner '000123')," [1,2,3] (scanner "000123"))
scanner5 = TestCase (assertEqual "for (scanner '0')," [0] (scanner "0"))
```

#### output :: BigNumber -> String

  > converts BN into String. Uses comprehension list to treat negative numbers and to process a BN.
  
```haskell
output bn
  | isNegBN bn = "-" ++ show (head bn * (-1)) ++ [head (show digit) | digit <- drop 1 bn]
  | otherwise = [head (show digit) | digit <- bn]
```

  > Testes

#### somaBN :: BigNumber -> BigNumber -> BigNumber

  > **somaBN'**: sums 2 BNs recursively using carry.

  > **somaBN**: checks base cases for sum operation and calls the appropriate functions to execute the operation (either somaBN' or subBN)

```haskell
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
```

  > Testes

#### subBN :: BigNumber -> BigNumber -> BigNumber

  > **subBN'**: subtracts 2 BNs recursively using carry

  > **subBN**: checks base cases for subtraction operation and calls the appropriate functions to execute the operation (either somaBN or subBN')

```haskell
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
```

  > Testes

#### mulBN :: BigNumber -> BigNumber -> BigNumber

  > EXPLANATION
```haskell
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

mulBN' :: BigNumber -> BigNumber -> BigNumber
mulBN' bn1 bn2 = foldl somaBN [0] resMulZeros
  where
    rbn1 = reverse bn1
    rbn2 = reverse bn2
    resMulZeros = head resMul : [resMul !! i ++ replicate i 0 | i <- [0..length resMul - 1], i > 0]
    resMul = [reverse (mulBN'' a 0 rbn1) | a <- rbn2]

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
```
  > Testes

#### divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)

  > divides 2 BNs using comprehension list that generates all multiples of the divisor. Uses the function **takeWhile** and **ltEqualBN** to stop the list generation.

```haskell
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
```

> **Testes**
```haskell
  - divBN [1,2,3,4,5,7,8,9,2] [2] (tempo superior a 10 minutos)
  - divBN [1,0] [2,0] >> Just ([0],[1,0])
  - divBN [1,0] [1,0] >> Just ([1],[0])
```

#### safeDivBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)

  > Ensures that divBN can divide a BN with zero, returning a Monad of type Maybe. When bn2 is zero, it returns Nothing. Otherwise, it calls divBN to execute the division.

```haskell
safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN bn1 bn2
  | isZeroBN bn2 = Nothing 
  | otherwise = Just(divBN bn1 bn2)
```

> **Testes**
```haskell
  - safeDivBN [1,2,3,4,5,7,8,9,2] [2]
  - safeDivBN [1,0] [2,0] >> Just ([0],[1,0])
  - safeDivBN [1,0] [1,0] >> Just ([1],[0])
  - safeDivBN [1,0] [0] >> Nothing
```

### Fib Module
  > Source Code: Fib.hs

  > Tests: Fib.test.hs

#### fibRec :: (Integral a) => a -> a

  > Normal recursion algorythm for Fibonacci. Two base cases and two recursion calls.

```haskell
fibRec n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fibRec (n - 1) + fibRec (n - 2)
```

  > Testes

#### fibLista2 :: Num b => Int -> b

  > In this version of FibLista, we use the function **foldl** so we can access the partial results of the list without needing to wait for the whole processing. It also ensures compiler optimization without bursting the stack. At the same time, the fold process reflects the change of state. The initial state is calculated step by step to get the final state, which is in line with the idea of ​​dynamic programming, so I think this is a recursive form of dynamic programming in functional programming.

```haskell
fibLista2' :: Num b => (b, b) -> p -> (b, b)
fibLista2' (a, b) _ = (b, a + b)

fibLista2 :: Num b => Int -> b
fibLista2 n = snd (foldl fibLista2' (1, 1) (replicate (n - 1) 0))
```

  > Testes

#### fibLista :: Num a => Int -> a

  > Calculates the nth Fibonacci number using the higher order function **map** to create the Fibonacci list with the sum of recursive call of the last two numbers on the list.The index operator **!!** is used to access the partial result, which is the nth number.

```haskell
fibLista :: Num a => Int -> a
fibLista n = fibLista !! n
  where
    fibLista = 1 : 1 : map f [2 ..]
    f n = fibLista !! (n - 1) + fibLista !! (n - 2) 
```

  > Testes

#### fibListaInfinita :: Num a => Int -> a

  > Calculates an infinite list of Fibonnacci numbers by producing the list of  corresponding sums of the ever growing Fibonnacci list with its tail. Uses the function **zipWith** to do that.

```haskell
fibListaInfinita :: Num a => Int -> a
fibListaInfinita n =
  let fib = 0 : 1 : zipWith (+) fib (tail fib)
   in fib !! n
```

  > Testes

#### fibRecBN :: String -> String

  > EXPLANATION

```haskell
fibRecBN' :: BigNumber -> BigNumber
fibRecBN' bn
  | bn `equalsBN` [0] = [1]
  | bn `equalsBN` [1] = [1]
  | otherwise = fibRecBN' (bn `subBN` [1]) `somaBN` fibRecBN' (bn `subBN` [2])

fibRecBN :: String -> String
fibRecBN n = output (fibRecBN' (scanner n))
```

  > Testes

#### fibListaBN :: String -> String

  > EXPLANATION

```haskell
fibListaBN n = lista !! n
  where
    lista = [1] : [1] : map fib (listaInfBN [2])
    fib n = (lista !! read (output (n `subBN` [2])))`somaBN` (lista !! read (output (n `subBN` [1])))
```

  > Testes  

#### fibListaInfinitaBN :: Int -> String

  > EXPLANATION

```haskell
fibListaInfinitaBN' n =
  let fib = [0] : [1] : zipWith somaBN fib (tail fib)
   in fib !! n
```

  > Testes

### Utils Module

#### truncateString :: String ->  String

 > EXPLANATION
```haskell
truncateString str 
  | str == "0" = "0"
  | otherwise = if isNegativeNumber then '-' : truncated else truncated
  where
    isNegativeNumber = head str == '-'
    truncated = dropWhile (== '0') (if isNegativeNumber then tail str else str)
```

#### xor :: Bool -> Bool -> Bool

 > EXPLANATION
```haskell
xor x y | x && not y = True
        | not x && y = True
        | otherwise = False
```

## Conclusion *(Exercice 4)*  

In exercice 1 we implemented three methods for calculating the nth Fibonacci number. A recursive version *(1.1)*, an optimized version *(1.2)* of the previous function using a list of partial results *(dynamic programming)*, and a version *(1.3)* that generates an infinite list with all Fibonacci numbers and returns the element of order n.

In these variations of exercise 1, the calculations of the algorithms were applied with the type Int that have values within the range of -2147483647 to 2147483647. So the biggest value that our functions can take is 46, because the 47th fibonacci number is 2971215073 (which is greater than the maximum number an *Int* can represent).

Integer can be considered as a superset of Int. This value is not bounded by any number, hence an Integer can be of any length without any limitation.

In exercise 3, we implemented the same versions of the first exercise, but the calcutions of the algorithms were applied with the type *BigNumber*, which is represented by a list of *Ints*, so each of its digit is, in fact, an *Int*. For this purpose we created an entire arithmetic module for this new type with all the arithmetic operations as well as many other auxiliary functions. In haskell we can have infinite lists, so the biggest value that our new functions can take is also infinite.

## Auxiliar Functions to BigNumber Module

#### minBN :: BigNumber -> BigNumber -> BigNumber

#### maxBN :: BigNumber -> BigNumber -> BigNumber

#### negBN :: BigNumber -> BigNumber

#### isZeroBN :: BigNumber -> Bool

#### isNegBN :: BigNumber -> Bool

#### isPosBN :: BigNumber -> Bool

#### absBN :: BigNumber -> BigNumber

#### gtBN :: BigNumber -> BigNumber -> Bool

#### ltBN :: BigNumber -> BigNumber -> Bool

#### ltEqualBN :: BigNumber -> BigNumber -> Bool

#### gtEqualBN :: BigNumber -> BigNumber -> Bool

#### equalsBN :: BigNumber -> BigNumber -> Bool

