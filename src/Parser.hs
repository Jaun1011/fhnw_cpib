module Parser () where

import Model (Token)

data Tree a
    = Tree Token

parseExpression :: [Token] -> Tree