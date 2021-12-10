{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Model
import Scanner (scanner)
import Model
import Control.Applicative
import GHC.RTS.Flags (MiscFlags(installSEHHandlers), ProfFlags (retainerSelector))
import Data.Type.Bool (Not)
import Data.Type.Equality (outer)
import Data.Char
import GHC.Base (VecElem(Int8ElemRep))

newtype Parser a =
    P {
        parse :: [Token] -> Maybe (a, [Token])
    }

instance Functor Parser where
    fmap g p = P $ \inp -> do
        (a, out) <- parse p inp
        return (g a, out)


instance Applicative Parser where
    pure v = P $ \inp -> Just (v, inp)
    pg <*> pa = P $ \inp -> do
        (g, out) <- parse pg inp
        (a, out') <- parse pa out
        return (g a, out')

instance Alternative Parser where
    empty = P $ const Nothing
    p <|> q = P $ \inp ->
        case parse p inp of
            Nothing  -> parse q inp
            Just (v, out) -> Just (v, out)

instance Monad Parser where
    pa >>= f = P (\inp ->
       case parse pa inp of
            Nothing      -> Nothing
            Just (a, rest) -> parse (f a) rest)


next :: (a -> b) -> [a] -> Maybe (b, [a])
next  _ [] = Nothing
next  fn (c:cs) =  Just (fn c, cs)


item :: Parser Token
item = P (next id)


sat :: (Token -> Bool) -> Parser Token
sat p = do
   -- parses item and gets access to char
   c <- item
   -- if the char = predikate give pure c back or with empty the failed parser
   if p c
      then return c
      else empty

-- consumes terminal
trm :: Terminal -> Parser Attirbute 
trm t = do 
    (tc, a) <- item
    if tc == t 
        then case a of Just a -> return a
        else empty


trmAttr :: Attirbute  -> Parser Attirbute 
trmAttr a = do 
    (_, ac) <- item
    case ac of
        Just ac ->
            if a == ac 
            then return ac
            else empty 



ident :: Parser String
ident =  P $ \(a:as) ->
    case a of
        (LITERAL, Just (StringType i)) -> Just (i, as)
        _ -> Nothing


digit :: Parser Int
digit = P $ \(t:ts) ->
    case t of
        (ALITERAL, Just (IntType i)) -> Just (i, ts)
        _ -> Nothing

data IExpr
  = IAliteal Int
  | IIdent String
  | IMonadic Attirbute IExpr 
  | IOpr Attirbute IExpr IExpr 
  | INone
  deriving ()


instance Show IExpr where
    show n = "\n1\t" ++ show' n 2 ++ "\n"
        
        where
            show' :: IExpr -> Int -> String 
            show' (IAliteal n) i    = "(IAliteral " ++ show n ++ ")"
            show' (IIdent n) i      = "(IIdent " ++ show n ++ ")"
            show' (IOpr n a b ) i    = print "IOpr" a b n i
            
            print :: String -> IExpr -> IExpr -> Attirbute -> Int -> String 
            print s a b attr i = "(" ++ s ++ "\n" 
                    ++ replicate i '\t'
                    ++ show' a (i+1) ++ "\n"
                    ++ replicate i '\t'
                    ++ show' b (i+1) 
                    ++ "\n"
                    ++ replicate i '\t'
                    ++ show attr
                    ++ ")"

exprP :: Parser IExpr
exprP = termRelP >>= exprP'
    where 
        exprP' :: IExpr -> Parser IExpr
        exprP' a = do 
            -- attr (LogicOperator OR)
            attr <- trm LOGICOPR
            b <- termRelP
            c <- exprP' b 

            return (IOpr attr c a)
            <|> do return a

termRelP :: Parser IExpr
termRelP = opt termAddP
    where 
        opt a =  
            IOpr <$> trm RELOPR <*> termAddP <*> a
            <|> a

termAddP :: Parser IExpr
termAddP = termMultP>>= opt
    where 
        opt a  = do         
            attr <- trm ADDOPR 
            b <- termMultP 
            opt (IOpr attr a b)
            
            <|> do return a




termMultP :: Parser IExpr
termMultP = factorP >>= opt 
    where 
        opt :: IExpr -> Parser IExpr
        opt a  = do         
            attr <- trm  MULTOPR
            b <- factorP
            opt (IOpr attr a b)

            <|> do return a

factorP :: Parser IExpr
factorP = IAliteal <$> digit 
    <|> trm LPAREN *> exprP <* trm RPAREN  
    <|> IIdent <$> ident
    <|> IMonadic <$> monadicOprP <*>  factorP

monadicOprP :: Parser Attirbute 
monadicOprP  = trmAttr (LogicOperator NOT) <|> trm ADDOPR

{-
return (IMul (IConstInt a) (IConstInt n))
termRelP :: Parser IExpr
termAddP :: Parser IExpr 
termFactP :: Parser IExpr
Just (ILogic 
    (ILogic (ILogic (IAliteal 3) INone Nothing
    ) (IAliteal 3) (Just OR)) (IAliteal 1) (Just AND),[])
     
-}




