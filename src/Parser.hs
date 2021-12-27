import ParsingLib ( Alternative((<|>)), Parser, trm)
import Model (Terminal(..), AritmeticOperator (MULTI))
import GHC.RTS.Flags (GCFlags(numa), ProfFlags (retainerSelector))


--expr ::= term expr + | term
--term ::= factor * term | factor
--factor ::= (expr) | int

expr :: Parser IExpr
expr = do x <- term 
          trm LOGICOPR 
          y <- expr
          return (IAdd x y)
        <|> term

term :: Parser IExpr
term = do x <- factor
          trm ADDOPR
          y <- term
          return (IMul x y)
        <|> factor

factor :: Parser IExpr
factor = do trm LPAREN
            x <- expr
            trm RPAREN 
            return x          
         <|> literal 

literal :: Parser IExpr
literal = do 
    lit <- some literal
    return (IIdent lit) 

data BoolExpr 
  = BConst Bool
  | BTrue BoolExpr
  | BFalse BoolExpr
  deriving Show

data CondExpr 
  = IF CondExpr CondExpr (Maybe CondExpr) 
  | IFELSE CondExpr CondExpr 

data IExpr 
  = IConstInt Int 
  | IIdent String 
  | IMul IExpr IExpr
  | IAdd IExpr IExpr 
  deriving Show



