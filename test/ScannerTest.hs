module ScannerTest (scannerTests) where

import Scanner (scanner)
import Test.HUnit ( assertEqual, runTestTT, Test(..), Assertion )
import Model (Terminal(..), Attirbute (..), Token)


scannerTestSet :: [(String, [Token])]
scannerTestSet =
  [("1234", [(ALITERAL, Just (StringType "1234"))])
  ,("asdf", [(LITERAL, Just (StringType "asdf"))])
  ,(";", [(SEMICOLON , Nothing)])
  ,("if", [(IF , Nothing)])
  ,("if true then asdf else 1234", [
      (IF,Nothing),(TRUE,Nothing),
      (THEN,Nothing),
      (LITERAL,Just (StringType "asdf")),
      (ELSE,Nothing),
      (ALITERAL,Just (StringType "1234"))]
    )
  ]

testcase :: (String, [Token]) -> Test
testcase (i,t) = TestCase (assertEqual ("scanner string: " ++ i) (scanner i) t)

scannerTests :: Test
scannerTests = TestList (map (TestLabel "scanner" . testcase) scannerTestSet)
