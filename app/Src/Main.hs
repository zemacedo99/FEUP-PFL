module Main where

import Fib
    ( fibRec,
      fibLista,
      fibListaInfinita,
      fibRecBN,
      fibListaBN,
      fibListaInfinitaBN )
import qualified Data.Map as Map
import Data.Maybe (fromJust)

welcome :: [Char]
welcome = "\nWelcome to the Fibonnacci Generator app!\n\n --- please choose which method to use ---"

menu :: [Char]
menu = "\n\t1 - fibRec\n\t2 - fibLista\n\t3 - fibListaInfinita\n\t4 - fibRecBN\n\t5 - fibListaBN\n\t6 - fibListaInfinitaBN"

input :: [Char]
input = "\nInput a number and receive a corresponding fibonnacci number:"

callFibRec :: IO ()
callFibRec = do
  putStrLn input
  x <- getLine
  print (fibRec (read x))

callFibLista :: IO ()
callFibLista = do
  putStrLn input
  x <- getLine
  print (fibLista (read x))

callfibListaInfinita :: IO ()
callfibListaInfinita = do
  putStrLn input
  x <- getLine
  print (fibListaInfinita (read x))

callfibRecBN :: IO ()
callfibRecBN = do
  putStrLn input
  x <- getLine
  print (fibRecBN x)

callfibListaBN :: IO ()
callfibListaBN = do
  putStrLn input
  x <- getLine
  print (fibListaBN (read x))

callfibListaInfinitaBN :: IO ()
callfibListaInfinitaBN = do
  putStrLn input
  x <- getLine
  print (fibListaInfinitaBN (read x))

chooseMethod :: [Char] -> IO ()
chooseMethod option = fromJust (Map.lookup option optionsMap)
  where
    optionsMap = Map.fromList
        [ ("1", callFibRec),
          ("2", callFibLista),
          ("3", callfibListaInfinita),
          ("4", callfibRecBN),
          ("5", callfibListaBN),
          ("6", callfibListaInfinitaBN)
        ]

main :: IO ()
main = do
  putStrLn welcome
  putStrLn menu
  x <- getLine
  chooseMethod x
