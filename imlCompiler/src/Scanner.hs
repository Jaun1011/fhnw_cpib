module Scanner
    ( scanner
    ) where

import Prelude (Show, Maybe (Just, Nothing), String, Int, snd, fst, ($), otherwise, (||), Char, Bool, Eq ((==)))
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
s1 x = 
    let n = split x isLiteral 
    in (snd n, (LITERAL, Just (StringType (fst n))))


isLiteral :: Char -> Bool 
isLiteral c = isDigit c || isAlpha c


-- split string by condition in (head, tail)
split :: String -> (Char -> Bool) -> (String , String)
split [] _ = ("", "")
split (a:as) fn
    | fn a = 
         let n = split as fn 
         in (a : fst n, snd n)
    | otherwise = ("", a:as)


