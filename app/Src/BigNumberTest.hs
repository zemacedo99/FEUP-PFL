import Test.HUnit
import BigNumber (scanner, output, somaBN, subBN, mulBN, divBN, safeDivBN)

-- xor1 :: Test
-- xor1 = TestCase (assertEqual "for (True xor True)," False (True `xor` True))


tests :: Test
tests = TestList [
  -- TestLabel "xor1" xor1, 
  ]

runTestsBigNumber :: IO Counts
runTestsBigNumber = runTestTT tests

