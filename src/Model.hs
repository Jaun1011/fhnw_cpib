{-# LANGUAGE TemplateHaskell #-}


module Model (Token, Terminal(..), Attirbute(..), transformKeyword, specialChars) where

import Prelude (Show, Maybe (Just, Nothing), String, Int, snd, fst, ($), otherwise, (||), Char, Bool (True), Eq ((==), (/=)), read, ($), (.), not)
import Data.Maybe
--import Language.Haskell.TH.Syntax (Callconv)
import Data.Int (Int32, Int64)
import List (find, split)

data Terminal
    = IDENT
    | CALL
    | PROGRAM
    | SKIP
    | COMMENT
    | ASSIGN
    | BECOMES
    | COPY
    | TYPEDEF
    | UNKNOWN
    | MODE
    | VAR | CONST 
    | DEBUGIN | DEBUGOUT
    | DO | WHILE
    | BOOL| TRUE | FALSE
    | FUN | RETURNS
    | IF | THEN | ELSE 
    | IN | OUT
    | INIT | INOUT
    | LOCAL | GLOBAL
    | PROC | REF
    | LPAREN | RPAREN
    | SEMICOLON | COMMA
    | LITERAL | ALITERAL
    | RELOPR | LOGICOPR | ARITMOPR | DIVOPR
    | ENDFUN | ENDIF | ENDPROC | ENDPROGRAM | ENDWHILE
    deriving(Show, Eq)

data Attirbute
    = RelOperator RelOperator
    | AritmeticOperator AritmeticOperator
    | LogicOperator LogicOperator
    | DivOperator DivOperator
    | IntType Int
    | StringType String
    deriving(Show, Eq)

data Type
    = INT32
    | INT64
    | INT1024
    deriving(Show, Eq)

data LogicOperator
    = AND
    | OR
    | XOR
    | NOT
    deriving(Show, Eq)

data RelOperator
    = GREATER
    | LESS
    | EQUAL
    | GREATER_EQUAL
    | LESS_EQUAL
    | NOT_EQUAL
    deriving(Show, Eq)

data AritmeticOperator
   = PLUS
   | MINUS
   | MULTI
   | DIV
   deriving(Show, Eq)

data DivOperator
    = DIV_E
   | DIV_F
   | DIV_T
   | MOD_E
   | MOD_F
   | MOD_T
   deriving(Show, Eq)
 
type Token = (Terminal, Maybe Attirbute)  

keywords :: [(String, Token)]
keywords = 
    [("if", (IF,Nothing))
    ,("then", (THEN,Nothing))
    ,("else", (ELSE,Nothing))
    ,("endif", (ENDIF,Nothing))
    ,("while", (WHILE,Nothing))
    ,("bool", (BOOL,Nothing))
    ,("call", (CALL, Nothing))
    ,("const", (CONST, Nothing))
    ,("copy", (COPY, Nothing))
    ,("debugin", (DEBUGIN, Nothing))
    ,("debugout",  (DEBUGOUT, Nothing))
    ,("do", (DO, Nothing))
    ,("endfun", (ENDFUN, Nothing))
    ,("endproc", (ENDPROC, Nothing))
    ,("endprogram", (ENDPROGRAM, Nothing))
    ,("endwhile", (ENDWHILE, Nothing))
    ,("false", (LITERAL, Nothing))
    ,("fun", (FUN, Nothing))
    ,("global", (GLOBAL, Nothing))
    ,("in", (IN, Nothing))
    ,("init", (INIT, Nothing))
    ,("inout", (INOUT, Nothing))
    ,("local", (LOCAL, Nothing))
    ,("out", (OUT, Nothing))
    ,("proc", (PROC, Nothing))
    ,("program" , (PROGRAM, Nothing))
    ,("ref", (REF, Nothing))
    ,("returns", (RETURNS, Nothing))
    ,("skip", (SKIP, Nothing))
    ,("true", (TRUE, Nothing))
    ,("var", (VAR, Nothing))
    ,("assign", (ASSIGN, Nothing))
    ,("lparen", (LPAREN, Nothing))
    ,("rparen", (RPAREN, Nothing))
    ,("becomes", (BECOMES, Nothing))
    ,("semicolon", (SEMICOLON, Nothing))
    ,("comma", (COMMA, Nothing))
    ,("relopr", (RELOPR, Nothing))
    ,("logicopr", (LOGICOPR, Nothing))
    ,("aritmopr", (ARITMOPR, Nothing))
    ,("literal", (LITERAL, Nothing))
    ,("aliteral", (ALITERAL, Nothing))
    ,("typedef", (TYPEDEF, Nothing))
    ,("unknown", (UNKNOWN, Nothing))]


transformKeyword :: Token -> Token
transformKeyword (LITERAL, Just (StringType s)) = keywordToken s 
transformKeyword t = t

keywordToken :: String -> Token
keywordToken s = case find keywords (\(k,t) -> s == k) of
    Just (_,t) -> t
    Nothing -> (LITERAL, Just (StringType s))


specialChars :: String -> (String, Maybe Token)
specialChars ('/':'\\':'?':cs) = (cs,Just (LOGICOPR, Just (LogicOperator AND)))
specialChars ('\\':'/':'?':cs) = (cs,Just (LOGICOPR, Just (LogicOperator OR)))

specialChars ('/':'/':cs) = let (s, e) = split cs  (/= '\n') in (e,Just (COMMENT, Just (StringType s)))
specialChars (':':'=':cs) = (cs,Just (ASSIGN, Nothing))
specialChars ('/':'=':cs) = (cs,Just (RELOPR, Just (RelOperator NOT_EQUAL)))
specialChars ('>':'=':cs) = (cs,Just (RELOPR, Just (RelOperator GREATER_EQUAL)))
specialChars ('<':'=':cs) = (cs,Just (RELOPR, Just (RelOperator LESS_EQUAL)))

specialChars ('=':cs) = (cs,Just (RELOPR, Just (RelOperator EQUAL)))
specialChars ('<':cs) = (cs,Just (RELOPR, Just (RelOperator LESS)))
specialChars ('>':cs) = (cs,Just (RELOPR, Just (RelOperator GREATER)))

specialChars ('+':cs) = (cs,Just (RELOPR, Just (AritmeticOperator PLUS)))
specialChars ('-':cs) = (cs,Just (RELOPR, Just (AritmeticOperator MINUS)))
specialChars ('*':cs) = (cs,Just (RELOPR, Just (AritmeticOperator MULTI)))
specialChars ('/':cs) = (cs,Just (RELOPR, Just (AritmeticOperator DIV)))

specialChars ('(':cs) = (cs,Just (LPAREN, Nothing))
specialChars (')':cs) = (cs,Just (RPAREN, Nothing))

specialChars (':':cs) = (cs,Just (TYPEDEF, Nothing))
specialChars (';':cs) = (cs,Just (SEMICOLON, Nothing))
specialChars (',':cs) = (cs,Just (COMMA, Nothing))
specialChars ('!':cs) = (cs,Just (DEBUGOUT, Nothing))
specialChars ('?':cs) = (cs,Just (DEBUGIN, Nothing))
specialChars ('%':cs) = (cs,Just (MODE, Nothing))
specialChars ('&':cs) = (cs,Just (LOGICOPR, Just (LogicOperator AND)))
specialChars ('^':cs) = (cs,Just (LOGICOPR, Just (LogicOperator XOR)))
specialChars ('|':cs) = (cs,Just (LOGICOPR, Just (LogicOperator OR)) )
specialChars ('~':cs) = (cs,Just (LOGICOPR, Just (LogicOperator NOT)))
specialChars n = (n, Nothing)
