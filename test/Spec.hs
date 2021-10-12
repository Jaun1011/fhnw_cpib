
import Scanner (scanner)
import Test.HUnit

test1 = TestCase (assertEqual "for (foo 3)," (1) (3))
tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
  runTestTT tests
  return ()