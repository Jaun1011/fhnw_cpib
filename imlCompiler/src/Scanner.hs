module Scanner
    ( scanner
    ) where

import Prelude (Show, Maybe (Just), String, Int)

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
    deriving(Show)

data Attirbute
    = RelOperator RelOperator 
    | IntegarType Int   
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
s0 ('<':'=':cs) = (RELOPR, Just (RelOperator LESS_EQUAL)) : s0 cs
s0 ('>':'=':cs) = (RELOPR, Just (RelOperator GREATER_EQUAL)) : s0 cs
s0 ('=':cs) = (RELOPR, Just (RelOperator EQUAL)) : s0 cs
s0 ('<':cs) = (RELOPR, Just (RelOperator LESS)) : s0 cs
s0 ('>':cs) = (RELOPR, Just (RelOperator GREATER)) : s0 cs
s0 (c:cs) = s0 cs
s0 "" = []


