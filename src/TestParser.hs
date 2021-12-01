{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import Model
import Scanner (scanner)
import Model
import Control.Applicative
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))
import Data.Type.Bool (Not)
import Data.Type.Equality (outer)
import Data.Char

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
item = P (next (\a -> a))


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
trm t1 = sat (\(t2,_) -> t1 == t2)


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

data Operator 
  = TIMES  
  | DIV_E | MOD_E | DIV_F | MOD_F | DIV_T | MOD_T
  | Plus | Minus
  | Lt | Ge | Eq | Ne | Gt | Le
  | Notopr | Cand | Cor

data IType 
  = Bool | Int64 | Int | Int32 | Int1024

data IFlowMode
  = In | Inout | Out

data IMechMode
  = Copy | Ref

data IChangeMode
  = Const | Var

data IValue
  = BoolVal Bool | Int64Val Int

data IProgram
  = IProgramm ICommand IParamLists IDeclaration ICommand --ident  [progParamList] [cpsDecl] [cpsCmd]

data IDeclaration
  = IDecl IDeclaration
  | IStoDecl IChangeMode IParamLists
  | IFunDecl ICommand IParamLists IDeclaration IDeclaration IDeclaration ICommand
  | IProcDecl ICommand IParamLists IDeclaration IDeclaration ICommand
  | IGlobImps IDeclaration IDeclaration
  | IGlobImp IFlowMode IChangeMode ICommand
  | ICpsDecl IDeclaration IDeclaration
  | ICpsStoDecl IDeclaration IDeclaration
  deriving ()

data IParamLists
  = IProgParamList  IParamLists IParamLists -- progParam progParam
  | IProgParam IFlowMode IChangeMode IParamLists -- flowmode changemode [typeIdent]
  | IParamList IParamLists IParamLists
  | IParam IFlowMode IMechMode IChangeMode IParamLists
  | ITypeIdent IExpr IParamLists [IParamLists]
  | IAtomType IType
  deriving ()

data ICommand
  =  ISkipCmd 
  | ICpsCmd ICommand ICommand
  | IGlobInits ICommand
  | IIdents ICommand ICommand
  | IIdent ICommand
  | IIdentChars String
  | IIdentArray ICommand ICommand
  | IArrayBrackets IExpr
  | IArrayLength 
  deriving ()

data IExpr
  = IAliteal Int
  | ILiteral ICommand
  | IRelOpr IExpr IExpr (Maybe Attirbute)
  | IMult IExpr IExpr  (Maybe Attirbute) 
  | ILogic IExpr IExpr (Maybe Attirbute) 
  | IAdd IExpr IExpr  (Maybe Attirbute)
  | INone
  deriving ()


instance Show IExpr where
    show n = "\n1\t" ++ show' n 2 ++ "\n"
        where
            show' :: IExpr -> Int -> String 
            show' (IAliteal n) i    = "(IAliteral " ++ show n ++ ")"
            show' (IRelOpr a b n) i = print "IRelOpr" a b i
            show' (IMult a b n) i   = print "IMult" a b i
            show' (IAdd a b n) i    = print "IAdd" a b i
            show' (ILogic a b n) i  = print "ILogic" a b i
            
            print :: String -> IExpr -> IExpr -> Int -> String 
            print s a b i = "(" ++ s ++ "\n" 
                    ++ replicate i '\t'
                    ++ show' a (i+1) ++ "\n"
                    ++ replicate i '\t'
                    ++ show' b (i+1)
                    ++ ")"

exprP :: Parser IExpr
exprP = termRelP >>= exprP'
    where 
        exprP' :: IExpr -> Parser IExpr
        exprP' a = do 
            -- attr (LogicOperator OR)
            (_, attr) <- trm LOGICOPR
            b <- termRelP
            c <- exprP' b 

            return (ILogic c a attr)
            <|> do return a

termRelP :: Parser IExpr
termRelP = termAddP >>= opt
    where 
        opt a = do 
            (_, attr) <- trm RELOPR
            b <- termAddP
            return (IRelOpr a b attr)
            <|> do return a

termAddP :: Parser IExpr
termAddP = termMultP>>= opt
    where 
        opt a  = do         
            (_, attr) <- trm ADDOPR 
            b <- termMultP 
            c <- opt b
            return (IAdd c a attr)
            <|> do return a

termMultP    :: Parser IExpr
termMultP = factorP >>= opt
    where 
        opt a  = do         
            (_, attr) <- trm  MULTOPR
            b <- factorP
            c <- opt b
            return (IMult c a attr)
            <|> do return a

factorP :: Parser IExpr
factorP = IAliteal <$> digit 
    <|> trm LPAREN *> exprP <* trm RPAREN  
    <|> ILiteral <$> identP


identP :: Parser ICommand
identP = IIdentChars <$> ident








{-
return (IMul (IConstInt a) (IConstInt n))
termRelP :: Parser IExpr
termAddP :: Parser IExpr 
termFactP :: Parser IExpr
Just (ILogic 
    (ILogic (ILogic (IAliteal 3) INone Nothing
    ) (IAliteal 3) (Just OR)) (IAliteal 1) (Just AND),[])
     
-}


