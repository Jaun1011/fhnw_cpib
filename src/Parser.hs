import ParsingLib


--expr ::= term expr + | term
--term ::= factor * term | factor
--factor ::= (expr) | int

inat :: Parser IExpr 
inat = do ds <- some digit
          return (IConst (read ds))



expr :: Parser IExpr
expr = do x <- term
          char '+'
          y <- expr
          return (IAdd x y)
        <|> term

term :: Parser IExpr
term = do x <- factor
          char '*'
          y <- term
          return (IMul x y)
        <|> factor

factor = do char '('
            x <- expr
            char ')'
            return x
           <|> inat

data BoolExpr = 
  BConst Bool
  | BTrue BoolExpr
  | BFalse BoolExpr
  deriving Show

data CondExpr = 
  IF CondExpr CondExpr (Maybe CondExpr)
  | IFELSE CondExpr CondExpr 



data IExpr = 
  IConst Int 
  | IMul IExpr IExpr
  | IAdd IExpr IExpr 
  deriving Show

a :: IExpr
a = IAdd (IMul (IConst 2) (IConst 3)) (IConst 4)

eval :: IExpr -> Int 
eval (IConst i) = i
eval (IMul lhs rhs) = eval lhs * eval rhs
eval (IAdd lhs rhs) = eval lhs + eval rhs

