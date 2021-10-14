module Parser () where

import Model (Token)
import Scanner (scanner)


parseExpr :: [Token] -> Expression