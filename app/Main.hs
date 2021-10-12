module Main where

import Scanner (scanner)

main :: IO ()
main = do
    text <- readFile "../test/programs/Factorial.iml"
    print $scanner text
