

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ParsingLib (
   Parser,
   parse,
   item,
   ident,
   digit,
   trm,
   trmA,
   trmAByAttr,
) where 


import Model
import Scanner (scanner)
import Model
import Control.Applicative
import GHC.RTS.Flags (MiscFlags(installSEHHandlers), ProfFlags (retainerSelector), ParFlags)
import Data.Type.Bool (Not)
import Data.Type.Equality (outer)
import Data.Char
import GHC.Base (VecElem(Int8ElemRep), Type)
import Control.Monad (when)
import GHC.Float (Floating(exp))
import Data.Maybe (Maybe(Nothing))
import Debug.Trace
import Data.Semigroup (option)
import Model (Terminal(FALSE, BECOMES, ENDPROGRAM))
import Data.Bool (Bool(False))
import GHC.Arr (cmpArray)
import System.IO (putStrLn)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Logger (info, logIdent)


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
trm :: Terminal -> Parser Token
trm t = do
    (tc, a) <- item
    -- logIdent 1 "trm" (tc, a)

    if tc == t
        then return (tc, a)
        else empty

trmA :: Terminal  -> Parser Attirbute
trmA t = do
    (_, attr) <- trm t
    --logIdent 1 "trmA" attr

    case attr of
        Just a -> return a
        _ -> empty

trmAByAttr :: Attirbute  -> Parser Attirbute
trmAByAttr a = do
    (_, ac) <- item
    logIdent 1  "trmAByAttr" ac


    case ac of
        Just ac ->
            if a == ac
            then return ac
            else empty

ident :: Parser String
ident =  P $ \(a:as) ->
    case a of
        (LITERAL, Just (StringType i)) -> do 
            logIdent 1  "ident" (i)
            Just (i, as)
        _ -> Nothing


digit :: Parser Int
digit = P $ \(t:ts) ->
    case t of
        (ALITERAL, Just (IntType i)) ->do
            logIdent 1  "digit" i
            Just (i, ts)
        _ -> Nothing


