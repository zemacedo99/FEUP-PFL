import Test.HUnit
import Utils ( truncateString, xor )

xor1 :: Test
xor1 = TestCase (assertEqual "for (True xor True)," False (True `xor` True))

xor2 :: Test
xor2 = TestCase (assertEqual "for (True xor True)," True (False `xor` True))

xor3 :: Test
xor3 = TestCase (assertEqual "for (True xor True)," False (False `xor` False))

truncateString1 :: Test
truncateString1 = TestCase (assertEqual "for (truncateString '0000100')," "100" (truncateString "0000100"))

truncateString2 :: Test
truncateString2 = TestCase (assertEqual "for (truncateString '-0000100')," "-100" (truncateString "0000100"))

tests :: Test
tests = TestList [
  TestLabel "xor1" xor1, 
  TestLabel "xor2" xor2, 
  TestLabel "xor3" xor3, 
  TestLabel "truncateString1" truncateString1,
  TestLabel "truncateString2" truncateString2 
  ]

runTestsUtils :: IO Counts
runTestsUtils = runTestTT tests