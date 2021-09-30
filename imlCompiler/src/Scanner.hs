module Scanner
    ( scanner
    ) where

import Prelude (Show, Maybe (Just, Nothing), String, Int, snd, fst, ($), otherwise, (||), Char, Bool, Eq ((==), (/=)), read, ($), (.))
import GHC.Unicode (isAlpha, isDigit)
import GHC.Float (fromRat'')
import Data.Maybe

data Terminal
    = IDENT
    | COMMENT
    | ASSIGN
    | NOT
    | LPAREN
    | RPAREN
    | BECOMES
    | SEMICOLON
    | COMMA
    | RELOPR
    | WHILE
    | LOGICOPR
    | ARITMOPR
    | SKIP
    | IF
    | THEN
    | ELSE
    | ENDIF
    | LITERAL
    | ALITERAL
    | TYPEDEF
    | UNKNOWN
    
    deriving(Show)

data Attirbute
    = RelOperator RelOperator
    | AritmeticOperator AritmeticOperator
    | LogicOperator LogicOperator
    | IntType Int
    | StringType String
    deriving(Show)




data LogicOperator
    = AND
    | OR
    deriving(Show)

data RelOperator
    = GREATER
    | LESS
    | EQUAL
    | GREATER_EQUAL
    | LESS_EQUAL
    deriving(Show)


data AritmeticOperator
   = PLUS
   | MINUS
   | MULTI
   | DIV
    deriving(Show)


type Token = (Terminal, Maybe Attirbute)

apastroph :: Char 
apastroph = '\''

scanner :: String -> [Token]
scanner a = kwt $ s0 a 


litAttr :: Attirbute -> String
litAttr (StringType a) = a

kwt :: [Token] -> [Token]
kwt [] = []
kwt ((LITERAL, attr):as) = keyword (litAttr $ fromJust attr) : kwt as
kwt (a:as) = a: kwt as



keyword :: String -> Token
keyword "if"    = (IF,Nothing)
keyword "then"  = (THEN,Nothing)
keyword "else"  = (ELSE,Nothing)
keyword "endif" = (ENDIF,Nothing)
keyword "while" = (WHILE,Nothing)
keyword c       = (LITERAL, Just $StringType c) 


-- 
s0 :: String -> [Token]
s0 [] = []

s0 ('/':'\\':'?':cs) = (LOGICOPR, Just (LogicOperator AND))     : s0 cs
s0 ('\\':'/':'?':cs) = (LOGICOPR, Just (LogicOperator OR))      : s0 cs

s0 ('/':'/':cs) = let (s, token) = s1_comment cs in token: s0 s
s0 (':':'=':cs) = (ASSIGN, Nothing) : s0 cs

s0 ('>':'=':cs) = (RELOPR, Just (RelOperator GREATER_EQUAL))    : s0 cs
s0 ('=':cs)     = (RELOPR, Just (RelOperator EQUAL))            : s0 cs
s0 ('<':cs)     = (RELOPR, Just (RelOperator LESS))             : s0 cs
s0 ('>':cs)     = (RELOPR, Just (RelOperator GREATER))          : s0 cs

s0 ('+':cs)     = (ARITMOPR, Just (AritmeticOperator PLUS))     : s0 cs
s0 ('-':cs)     = (ARITMOPR, Just (AritmeticOperator MINUS))    : s0 cs
s0 ('/':cs)     = (ARITMOPR, Just (AritmeticOperator DIV))      : s0 cs
s0 ('*':cs)     = (ARITMOPR, Just (AritmeticOperator MULTI))    : s0 cs

s0 ('(':cs)     = (LPAREN, Nothing)     : s0 cs
s0 (')':cs)     = (RPAREN, Nothing)     : s0 cs

s0 (':':cs)     = (TYPEDEF, Nothing)    : s0 cs
s0 (';':cs)     = (SEMICOLON, Nothing)  : s0 cs
s0 (',':cs)     = (COMMA, Nothing)      : s0 cs

s0 (' ':cs)     = s0 cs
s0 ('\n':cs)    = s0 cs
s0 ('\t':cs)    = s0 cs

s0 all@(c:cs)
    | isAlpha c = let (a,b) = s1_literal all in b : s0 a
    | isDigit c = let (a,b) = s2_number all in b : s0 a
    | otherwise = (UNKNOWN, Just (StringType [c])): s0 cs


-- gives back comment
s1_comment :: String -> (String, Token)
s1_comment x = transformSplit (split x (/= '\n')) COMMENT StringType

-- state checks string literals
s1_literal :: String -> (String, Token)
s1_literal x = transformSplit (split x isLiteral) LITERAL StringType

-- state checks numbers
s2_number :: String -> (String, Token)
s2_number x = transformSplit (litNum, b) ALITERAL (\n -> IntType (read n :: Int))
    where 
        (a,b) = split x isNumber
        litNum = remove a apastroph

transformSplit :: (String , String) -> Terminal -> (String -> Attirbute) -> (String, Token)
transformSplit (a, b) attr fn = (b, (attr, Just (fn a)))

-- is character a digit or a number
isLiteral :: Char -> Bool
isLiteral c = isDigit c || isAlpha c || c == apastroph

isNumber :: Char -> Bool
isNumber c = isDigit c || isAlpha c || c == apastroph


-- split string by condition in (head, tail)
split :: String -> (Char -> Bool) -> (String , String)
split [] _ = ("", "")
split (a:as) fn
    | fn a =
         let (x, y) = split as fn
         in (a : x, y)
    | otherwise = ("", a:as)


-- removes character from string
remove :: String -> Char ->  String
remove [] _ = "" 
remove (a:as) c 
    | a == c = remove as c
    | otherwise = a : remove as c

