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
    let (fn, t) = fnLiteral a
        (s, e) = split all fn
    in (e, (t, Just (StringType s)))


isLiteral :: Char -> Bool
isLiteral c = isNumber c || isAlpha c

isNumber :: Char -> Bool 
isNumber c =  isDigit c || c == '\''

fnLiteral :: Char -> (Char -> Bool, Terminal)
fnLiteral c 
    | isAlpha c = (isLiteral, LITERAL)
    | isDigit c = (isNumber, ALITERAL)
    | otherwise = (const True, UNKNOWN)
    
