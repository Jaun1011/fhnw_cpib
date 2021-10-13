import Scanner (scanner)
import Test.HUnit ( assertEqual, runTestTT, Test(..), Assertion )
import Model (Terminal(..), Attirbute (..), Token)



test_scanner :: String -> [Token] -> Test
test_scanner i t = TestCase (assertEqual ("scanner string: " ++ i) (scanner i) t)

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

testlabel :: (String, [Token]) -> Test
testlabel (s,t) = TestLabel "scanner" (test_scanner s t) 

tests :: Test
tests = TestList (map testlabel scannerTestSet)

main :: IO ()
main = do
  runTestTT tests
  return ()