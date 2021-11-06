module ParserTest () where

import Test.HUnit ( assertEqual, runTestTT, Test(..), Assertion )
import Model (Terminal(..), Attirbute (..), Token)

import Parser (Tree, parser)

