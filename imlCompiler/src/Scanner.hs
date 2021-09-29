module Scanner
    ( scanner
    ) where

import Prelude (Show, Maybe (Just, Nothing), String, Int, snd, fst, ($), otherwise)
import GHC.Unicode (isAlpha, isDigit)
import Data.Sequence.Internal.Sorting (QList(Nil))

data Terminal
    = IDENT
    | NOT
    | LPAREN
    | RPAREN
    | BECOMES
    | SEMICOLON
    | RELOPR
    | SKIP
    | IF
    | THEN
    | ELSE
    | LITERAL
    deriving(Show)

data Attirbute
    = RelOperator RelOperator
    | IntegarType Int
    | StringType String
    deriving(Show)

data RelOperator
    = GREATER
    | LESS
    | EQUAL
    | GREATER_EQUAL
    | LESS_EQUAL
    deriving(Show)



type Token = (Terminal, Maybe Attirbute)

scanner :: String -> [Token]
scanner = s0

s0 :: String -> [Token]
s0 [] = []
s0 ('<':'=':cs) = (RELOPR, Just (RelOperator LESS_EQUAL))    : s0 cs
s0 ('>':'=':cs) = (RELOPR, Just (RelOperator GREATER_EQUAL)) : s0 cs
s0 ('=':cs)     = (RELOPR, Just (RelOperator EQUAL))         : s0 cs
s0 ('<':cs)     = (RELOPR, Just (RelOperator LESS))          : s0 cs
s0 ('>':cs)     = (RELOPR, Just (RelOperator GREATER))       : s0 cs
s0 (c:cs)
    | isAlpha c =
        let x = s1 (c:cs)
        in snd x : s0 (fst x)


s1 :: String -> (String, Token)
s1 x = ("", (LITERAL, Just (StringType $s1_literal' x)))


s1_remain' :: String -> String
s1_remain' (a:as)
    | isAlpha a = s1_remain' as
    | isDigit a = s1_remain' as
    | otherwise  = as

s1_literal' :: String -> String
s1_literal' (a:as)
    | isAlpha a = a : s1_literal' as
    | isDigit a = a : s1_literal' as
    | otherwise  = ""


