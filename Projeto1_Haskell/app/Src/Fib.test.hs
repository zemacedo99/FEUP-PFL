import Test.HUnit
import Fib (fibRec, fibLista, fibLista2, fibListaInfinita, fibRecBN, fibListaBN, fibListaInfinitaBN)

resInt :: [Int]
resInt = [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269]

resString :: [String]
resString = ["1","1","2","3","5","8","13","21","34","55","89","144","233","377","610","987","1597","2584","4181","6765","10946","17711","28657","46368","75025","121393","196418","317811","514229","832040","1346269"]

resString15 :: [String]
resString15 = ["1","1","2","3","5","8","13","21","34","55","89","144","233","377","610","987"]

-- *** fibRec ***
fibRec1 :: Test
fibRec1 = TestCase (assertEqual "Should give the first 30 fibonnacci numbers" resInt ([fibRec n | n <- [0..30]]))

-- *** fibLista ***
fibLista1 :: Test
fibLista1 = TestCase (assertEqual "Should give the first 30 fibonnacci numbers" resInt ([fibLista n | n <- [0..30]]))

-- *** fibLista2 ***
fibLista21 :: Test
fibLista21 = TestCase (assertEqual "Should give the first 30 fibonnacci numbers" resInt ([fibLista2 n | n <- [0..30]]))

-- *** fibListaInfinita ***
fibListaInfinita1 :: Test
fibListaInfinita1 = TestCase (assertEqual "Should give the first 30 fibonnacci numbers" resInt ([fibListaInfinita n | n <- [0..30]]))

-- *** fibRecBN ***
fibRecBN1 :: Test
fibRecBN1 = TestCase (assertEqual "Should give the first 15 fibonnacci numbers" resString15 ([fibRecBN (show n) | n <- [0..15]]))

fibRecBN2 :: Test
fibRecBN2 = TestCase (assertEqual "Should give the first 30 fibonnacci numbers" resString ([fibRecBN (show n) | n <- [0..30]]))


-- *** fibListaBN ***
fibListaBN1 :: Test
fibListaBN1 = TestCase (assertEqual "Should give the first 30 fibonnacci numbers" resString ([fibListaBN n | n <- [0..30]]))

-- *** fibListaInfinitaBN ***
fibListaInfinitaBN1 :: Test
fibListaInfinitaBN1 = TestCase (assertEqual "Should give the first 30 fibonnacci numbers" resString ([fibListaInfinitaBN n | n <- [0..30]]))

tests :: Test
tests = TestList [
  -- TestLabel "fibRecBN2" fibRecBN2,
  TestLabel "fibRecBN1" fibRecBN1,
  TestLabel "fibRec1" fibRec1,
  TestLabel "fibLista1" fibLista1,
  TestLabel "fibLista21" fibLista21,
  TestLabel "fibListaInfinita1" fibListaInfinita1,
  TestLabel "fibListaInfinitaBN1" fibListaInfinitaBN1
  ]

runTestsFib :: IO Counts
runTestsFib = runTestTT tests