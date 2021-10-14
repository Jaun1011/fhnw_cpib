module Parser (Tree, expr) where

import Model (Token, Terminal(..))
import Scanner (scanner)




data Tree 
    = Nil 
    | Node Token Token Tree 
    | Leave Token 


instance Show Tree where
    show Nil = ""
    show (Node t1 t2 n) = show t1 ++ show t2 ++ "\n\t (- " ++ show n ++ "\n -)"
    show (Leave t) = show t ++ "\n"



exprLiteral :: Terminal -> Bool
exprLiteral t = t == LITERAL || t == ALITERAL

expr :: [Token] -> ([Token], Tree)
expr [] = ([], Nil)
expr (t1@(LITERAL,_):t2@(RELOPR,_):ts) = let (rs, n) = expr ts in (rs, Node t1 t2 n)
expr (t1@(ALITERAL,_):t2@(RELOPR,_):ts) = let (rs, n) = expr ts in (rs, Node t1 t2 n)
expr (t1@(RELOPR,_):t2@(LITERAL,_):ts) = let (rs, n) = expr ts in (rs, Node t1 t2 n)
expr (t1@(RELOPR,_):t2@(ALITERAL,_):ts) = let (rs, n) = expr ts in (rs, Node t1 t2 n)
expr (t1@(LITERAL,_):ts) = (ts, Leave t1)
expr (t1@(ALITERAL,_):ts) = (ts, Leave t1)
expr (t:ts) = (ts, Nil)


