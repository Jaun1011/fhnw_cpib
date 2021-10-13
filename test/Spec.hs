import Scanner (scanner)

import ScannerTest (scannerTests)
import Test.HUnit ( assertEqual, runTestTT, Test(..), Assertion )
import Model (Terminal(..), Attirbute (..), Token)

main :: IO ()
main = do
  runTestTT scannerTests
  return ()