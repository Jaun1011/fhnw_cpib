module Scanner
    ( scanner
    ) where


import GHC.Unicode (isAlpha, isDigit)

import Model (Token,Terminal(..), Attirbute(..),specialChars, transformKeyword )
import List (split)


scanner :: String -> [Token]
scanner [] = []
scanner (' ':as) = scanner as
scanner ('\n':as) = scanner as
scanner ('\t':as) = scanner as
scanner a =
    case specialChars a of
        (as, Just t) -> t : scanner as
        (as, Nothing) -> 
            let (rs, t) = scannerLiteral as 
            in transformKeyword t : scanner rs

scannerLiteral :: String -> (String, Token)
scannerLiteral [] = ([], (UNKNOWN ,Nothing))
scannerLiteral all@(a:as) =
    let (fn, term) = firstCharLiteral a
        (value, rest) = split all fn
    in (rest, integerTransform value term)

integerTransform :: String -> Terminal -> Token
integerTransform a ALITERAL = (ALITERAL, Just (IntType (read a)))
integerTransform a LITERAL = (LITERAL, Just (StringType a))
integerTransform a b = (UNKNOWN, Nothing)
    
isLiteral :: Char -> Bool
isLiteral c = isNumber c || isAlpha c

isNumber :: Char -> Bool 
isNumber c =  isDigit c || c == '\''

firstCharLiteral :: Char -> (Char -> Bool, Terminal)
firstCharLiteral c 
    | isAlpha c = (isLiteral, LITERAL)
    | isDigit c = (isNumber, ALITERAL)
    | otherwise = (const False, UNKNOWN)
    
