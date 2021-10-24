{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
module ParsingLib (module ParsingLib, module Control.Applicative) where

import Control.Applicative
import Data.Char
import Scanner
import Model
import GHC.TypeLits (AppendSymbol)


-- Takes a string and returns a maybe of  "a" and the string which wasn't consumed
newtype Parser a = P { parse :: String -> Maybe (a, String)}


-- Functor needed to map multiple parser at each other
instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                           Nothing     -> Nothing 
                           Just (a, out) -> Just (g a, out))

-- applicative instace to chain parsers
instance Applicative Parser where
   --pure :: a -> Parser a
   pure v = P (\inp -> Just (v, inp))
   -- <*> :: Parser (a->b) -> Parser a -> Parser b
   pg <*> pa = P (\inp -> 
      case parse pg inp of
         Nothing     -> Nothing
         Just (g,out)-> case parse pa out of
                           Nothing        -> Nothing 
                           Just (a, out') -> Just (g a, out'))

-- alternative has two methods "empty" and "choice -> takes 2 things an chooses one"
-- Making choices
instance Alternative Parser where 
   --empty :: Parser a
   empty = P (\inp -> Nothing )
   -- (<|>) :: Parser a -> Parser a -> Parser a
   -- try either p or q if p works don't look at q
   p <|> q = P (\inp -> 
      case parse p inp of 
         Nothing       -> parse q inp   -- If Parser p fails
         Just (v, out) -> Just (v,out)) -- If parser p succeeds

-- monad is used to remove case based code duplication, basically composition of even func
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f = P (\inp -> 
       case parse pa inp of
          Nothing      -> Nothing 
          Just (a, rest) -> parse (f a) rest)

-- parses a Char if predicate p is evaluated as true
sat :: (Char -> Bool ) -> Parser Char 
sat p = do 
   -- parses item and gets access to char
   c <- item
   -- if the char = predikate give pure c back or with empty the failed parser
   if p c then return c else empty

-- parses any char
item :: Parser Char
item = P (\inp -> case inp of
                     (c:cs) -> Just (c, cs)
                     _      -> Nothing)

-- parses only diggits
digit :: Parser Char
digit = sat isDigit

-- parses a char if it matches the given c
char :: Char -> Parser Char
char c = sat (==c)

-- parses natural numbers string to int values
nat :: Parser Int 
nat = fmap read (some digit)

-- parses to whole numbers -> takes the char and parses + negates the '-'
-- (whole numbers are a nat or a mit with '-' beforehand)
int :: Parser Int 
int = nat 
    <|> fmap (*(-1)) (char '-' *> nat)

-- builds a parser that consumes the defined string
string :: String -> Parser String
string []      = pure []
string (c:cs)  = pure (:) <*> char c <*> string cs

-- Handling spacing
space :: Parser ()
space = fmap (\_ -> ()) (many (sat isSpace))

-- token Is a primitive that ignores any space before and after p
token :: Parser a -> Parser a
token p = pure (\_ r _ -> r) <*> space <*> p <*> space



