import Test.HUnit
import BigNumber (scanner, output, somaBN, subBN, mulBN, divBN, safeDivBN)

-- *** scanner ***
scanner1 :: Test
scanner1 = TestCase (assertEqual "for (scanner '123')," [1,2,3] (scanner "123"))

scanner2 :: Test
scanner2 = TestCase (assertEqual "for (scanner '-123')," [-1,2,3] (scanner "-123"))

scanner3 :: Test
scanner3 = TestCase (assertEqual "for (scanner '-000123')," [-1,2,3] (scanner "-000123"))

scanner4 :: Test
scanner4 = TestCase (assertEqual "for (scanner '000123')," [1,2,3] (scanner "000123"))

scanner5 :: Test
scanner5 = TestCase (assertEqual "for (scanner '0')," [0] (scanner "0"))

-- *** output ***
output1 :: Test
output1 = TestCase (assertEqual "for (output [1,2,3])," "123" (output [1,2,3]))

output2 :: Test
output2 = TestCase (assertEqual "for (output [-1,2,3])," "-123" (output [-1,2,3]))

output3 :: Test
output3 = TestCase (assertEqual "for (output [0])," "0" (output [0]))

-- *** somaBN ***
somaBN1 :: Test
somaBN1 = TestCase (assertEqual "for ([2] `somaBN` [-2,2])," [-2,0] ([2] `somaBN` [-2,2]))

somaBN2 :: Test
somaBN2 = TestCase (assertEqual "for ([-2] `somaBN` [2,2])," [2,0] ([-2] `somaBN` [2,2]))

somaBN3 :: Test
somaBN3 = TestCase (assertEqual "for ([-2] `somaBN` [-2,2])," [-2,4] ([-2] `somaBN` [-2,2]))

somaBN4 :: Test
somaBN4 = TestCase (assertEqual "for ([2] `somaBN` [2,2])," [2,4] ([2] `somaBN` [2,2]))

somaBN5 :: Test
somaBN5 = TestCase (assertEqual "for ([2,2] `somaBN` [-2])," [2,0] ([2,2] `somaBN` [-2]))

somaBN6 :: Test
somaBN6 = TestCase (assertEqual "for ([-2,2] `somaBN` [2])," [-2,0] ([-2,2] `somaBN` [2]))

somaBN7 :: Test
somaBN7 = TestCase (assertEqual "for ([-2,2] `somaBN` [-2])," [-2,4] ([-2,2] `somaBN` [-2]))

somaBN8 :: Test
somaBN8 = TestCase (assertEqual "for ([2,2] `somaBN` [2])," [2,4] ([2,2] `somaBN` [2]))


-- *** subBN ***

subBN1 :: Test
subBN1 = TestCase (assertEqual "for ([2] `subBN` [-2,2])," [2,4] ([2] `subBN` [-2,2]))

subBN2 :: Test
subBN2 = TestCase (assertEqual "for ([-2] `subBN` [2,2])," [-2,4] ([-2] `subBN` [2,2]))

subBN3 :: Test
subBN3 = TestCase (assertEqual "for ([-2] `subBN` [-2,2])," [2,0] ([-2] `subBN` [-2,2]))

subBN4 :: Test
subBN4 = TestCase (assertEqual "for ([2] `subBN` [2,2])," [-2,0] ([2] `subBN` [2,2]))

subBN5 :: Test
subBN5 = TestCase (assertEqual "for ([2,2] `subBN` [-2])," [2,4] ([2,2] `subBN` [-2]))

subBN6 :: Test
subBN6 = TestCase (assertEqual "for ([-2,2] `subBN` [2])," [-2,4] ([-2,2] `subBN` [2]))

subBN7 :: Test
subBN7 = TestCase (assertEqual "for ([-2,2] `subBN` [-2])," [-2,0] ([-2,2] `subBN` [-2]))

subBN8 :: Test
subBN8 = TestCase (assertEqual "for ([2,2] `subBN` [2])," [2,0] ([2,2] `subBN` [2]))

subBN9 :: Test
subBN9 = TestCase (assertEqual "for ([1] `subBN` [2])," [-1] ([1] `subBN` [2]))

subBN10 :: Test
subBN10 = TestCase (assertEqual "for ([3,0] `subBN` [1])," [2,9] ([3,0] `subBN` [1]))

subBN11 :: Test
subBN11 = TestCase (assertEqual "for ([1] `subBN` [3,0])," [-2,9] ([1] `subBN` [3,0]))

subBN12 :: Test
subBN12 = TestCase (assertEqual "for ([2,2] `subBN` [1,0,0])," [-7,8] ([2,2] `subBN` [1,0,0]))

subBN13 :: Test
subBN13 = TestCase (assertEqual "for ([2,6,2] `subBN` [1,5,8])," [1,0,4] ([2,6,2] `subBN` [1,5,8]))

-- *** mulBN ***
mulBN1 :: Test
mulBN1 = TestCase (assertEqual "([0] `mulBN` [1,2,3])," [0] ([0] `mulBN` [1,2,3]))

mulBN2 :: Test
mulBN2 = TestCase (assertEqual "([1,2,3] `mulBN` [0])," [0] ([1,2,3] `mulBN` [0]))

mulBN3 :: Test
mulBN3 = TestCase (assertEqual "([1] `mulBN` [1,2,3])," [1,2,3] ([1] `mulBN` [1,2,3]))

mulBN4 :: Test
mulBN4 = TestCase (assertEqual "([1,2,3] `mulBN` [1])," [1,2,3] ([1,2,3] `mulBN` [1]))

mulBN5 :: Test
mulBN5 = TestCase (assertEqual "([-1] `mulBN` [1,2,3])," [-1,2,3] ([-1] `mulBN` [1,2,3]))

mulBN6 :: Test
mulBN6 = TestCase (assertEqual "([1,2,3] `mulBN` [-1])," [-1,2,3] ([1,2,3] `mulBN` [-1]))

mulBN7 :: Test
mulBN7 = TestCase (assertEqual "([1,2,3] `mulBN` [1,0])," [1,2,3,0] ([1,2,3] `mulBN` [1,0]))

mulBN8 :: Test
mulBN8 = TestCase (assertEqual "([1,2,3] `mulBN` [-1,0])," [-1,2,3,0] ([1,2,3] `mulBN` [-1,0]))

mulBN9 :: Test
mulBN9 = TestCase (assertEqual "([2] `mulBN` [5])," [1,0] ([2] `mulBN` [5]))

mulBN10 :: Test
mulBN10 = TestCase (assertEqual "([-2] `mulBN` [5])," [-1,0] ([-2] `mulBN` [5]))

mulBN11 :: Test
mulBN11 = TestCase (assertEqual "([-9,9,9,9,9,9,9] `mulBN` [-9,9,9,9,9,9,9])," [9,9,9,9,9,9,8,0,0,0,0,0,0,1] ([-9,9,9,9,9,9,9] `mulBN` [-9,9,9,9,9,9,9]))

-- *** divBN ***
divBN1 :: Test
divBN1 = TestCase (assertEqual "for ([1,0] `divBN` [2])," ([5],[0]) ([1,0] `divBN` [2]))

divBN2 :: Test
divBN2 = TestCase (assertEqual "for ([0] `divBN` [1])," ([0],[0]) ([0] `divBN` [1]))

divBN3 :: Test
divBN3 = TestCase (assertEqual "for ([2] `divBN` [1,0])," ([0],[2]) ([2] `divBN` [1,0]))

divBN4 :: Test
divBN4 = TestCase (assertEqual "for ([1] `divBN` [2])," ([0],[1]) ([1] `divBN` [2]))

divBN5 :: Test
divBN5 = TestCase (assertEqual "for ([1,2,3,4] `divBN` [5])," ([2,4,6],[4]) ([1,2,3,4] `divBN` [5]))

divBN6 :: Test
divBN6 = TestCase (assertEqual "for ([1,2,3,4] `divBN` [4,3,2])," ([2],[3,7,0]) ([1,2,3,4] `divBN` [4,3,2]))

divBN7 :: Test
divBN7 = TestCase (assertEqual "for ([1,2,3,4] `divBN` [1,2,3,4])," ([1],[0]) ([1,2,3,4] `divBN` [1,2,3,4]))

-- *** safeDivBN ***
safeDivBN1 :: Test
safeDivBN1 = TestCase (assertEqual "for ([2] `divBN` [0])," Nothing ([2] `safeDivBN` [0]))


tests :: Test
tests = TestList [
  TestLabel "scanner1" scanner1, 
  TestLabel "scanner2" scanner2, 
  TestLabel "scanner3" scanner3, 
  TestLabel "scanner4" scanner4,
  TestLabel "scanner5" scanner5,

  TestLabel "output1" output1, 
  TestLabel "output2" output2, 
  TestLabel "output3" output3,

  TestLabel "subBN1" subBN1,
  TestLabel "subBN2" subBN2,
  TestLabel "subBN3" subBN3,
  TestLabel "subBN4" subBN4,
  TestLabel "subBN5" subBN5,
  TestLabel "subBN6" subBN6,
  TestLabel "subBN7" subBN7,
  TestLabel "subBN8" subBN8,
  TestLabel "subBN9" subBN9,
  TestLabel "subBN10" subBN10,
  TestLabel "subBN11" subBN11,
  TestLabel "subBN12" subBN12,
  TestLabel "subBN13" subBN13,

  TestLabel "somaBN1" somaBN1,
  TestLabel "somaBN2" somaBN2,
  TestLabel "somaBN3" somaBN3,
  TestLabel "somaBN4" somaBN4,
  TestLabel "somaBN5" somaBN5,
  TestLabel "somaBN6" somaBN6,
  TestLabel "somaBN7" somaBN7,
  TestLabel "somaBN8" somaBN8,

  TestLabel "mulBN1" mulBN1,
  TestLabel "mulBN2" mulBN2,
  TestLabel "mulBN3" mulBN3,
  TestLabel "mulBN4" mulBN4,
  TestLabel "mulBN5" mulBN5,
  TestLabel "mulBN6" mulBN6,
  TestLabel "mulBN7" mulBN7,
  TestLabel "mulBN8" mulBN8,
  TestLabel "mulBN9" mulBN9,
  TestLabel "mulBN10" mulBN10,
  TestLabel "mulBN11" mulBN11,

  TestLabel "divBN1" divBN1,
  TestLabel "divBN2" divBN2,
  TestLabel "divBN3" divBN3,
  TestLabel "divBN4" divBN4,
  TestLabel "divBN5" divBN5,
  TestLabel "divBN6" divBN6,
  TestLabel "divBN7" divBN7,
  
  TestLabel "safeDivBN1" safeDivBN1
  ]

runTestsBigNumber :: IO Counts
runTestsBigNumber = runTestTT tests

