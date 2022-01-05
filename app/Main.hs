module Main where

import Scanner (scanner)

main :: IO ()
main = do
    text <- readFile "./test/programs/array_sample.iml"
    print $scanner text
