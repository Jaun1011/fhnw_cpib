module Main where

import Scanner

main :: IO ()
main = do
    text <- readFile "../programs/Factorial.iml"
    print $scanner text
