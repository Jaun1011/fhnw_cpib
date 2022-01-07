{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser (
  IExpr(..),
  IParameter(..),
  IDecl(..),
  ICmd(..),
  parseProgram,
  parseExpression,
  parseCmds,
  paramsToDelc
) where

import Model
import ParsingLib
import Scanner (scanner)
import Model
import Control.Applicative ( Alternative((<|>), empty) )
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
import ParsingLib (Parser(parse))
import Debug.Trace (trace)
import Utils.Logger (info)

data IParameter
    = INoParameter
    | IProgParams IParameter IParameter

    | IParams IParameter IParameter
    | IParam Attirbute Attirbute Attirbute IDecl

    | IGlobalImp Attirbute Attirbute IDecl
    | IStoreParams IParameter IDecl

    | ILocalImps IParameter IParameter

    deriving (Eq,Show)

data IDecl
    = IDeclItem IDecl IDecl
    | IStore Attirbute IDecl
    | IFunc String IParameter IDecl IParameter IDecl ICmd -- (name param return global local commands)
    | IProc String IParameter IParameter IDecl ICmd
    | IType String Attirbute
    | IArrayType String IExpr Attirbute
    | IProg String IParameter IDecl ICmd
    | INoDecl
    deriving (Eq,Show)

data ICmd
    = IBecomes IExpr IExpr
    | IIf IExpr ICmd ICmd
    | ICmds ICmd ICmd
    | ISkip
    | IWhile IExpr ICmd
    | IDebugIn IExpr
    | IDebugOut IExpr
    | ICaller String IExpr
    deriving (Eq,Show)

data IExpr
    = IAliteral Int
    | ILiteral String Bool -- name , is init?
    | ILiteralArray String IExpr-- name , is init?
    | IArrayLength String 
    | IMonadic Attirbute IExpr
    | IOpr Attirbute IExpr IExpr
    | IExprList String IExpr
    | IExprListParams IExpr IExpr
    | INone
    deriving (Eq)

instance Show IExpr where
    show n = "\n\t" ++ show' n 2 ++ "\n"

        where
            show' :: IExpr -> Int -> String
            show' INone i           = "(INone)"
            show' (IAliteral n) i    = "(IAliteral " ++ show n ++ ")"
            show' (IArrayLength n) i    = "(IArrayLength " ++ show n ++ ")"
            show' (ILiteral n b) i  = "(ILiteral " ++ show n ++ " " ++ show b ++ ")"
            show' (ILiteralArray n b) i  = "(ILiteralArray " ++ show n ++ " " ++ show b ++  ")"

            show' (IExprList a b) i    = "(IExprList " ++ show a ++ show b ++  ")"
            show' (IExprListParams a b) i    =   printExprList "IExprListParams" a b  i


            show' (IMonadic a b) i    =  print "IMonadic" b INone a  i


            show' (IOpr n a b ) i    = print "IOpr" a b n i
            printExprList :: String -> IExpr -> IExpr -> Int -> String
            printExprList s a b i = "(" ++ s ++ "\n"
                    ++ replicate i '\t'
                    ++ show' a (i+1) ++ "\n"
                    ++ replicate i '\t'
                    ++ show' b (i+1)
                    ++ ")"

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

paramsToDelc :: IParameter -> IDecl
paramsToDelc (IParams a b) = IDeclItem (paramsToDelc a) (paramsToDelc b)
paramsToDelc (IParam _ _ _ d) = d 



parseExpression :: [Token] -> IExpr
parseExpression ts = 
        case parse exprP fts of
            Just (a,[]) -> a
            Just (a, n)  -> error $"parser error" ++ show n
            otherwise -> error "nullerror"
      where fts = filter (\(a,b) -> a /= COMMENT ) ts


parseProgram :: [Token] -> IDecl
parseProgram ts = 
        case parse programP fts of
            Just (a,[]) -> a
            Just (a, n)  -> error $"parser error" ++ show n
            otherwise -> error "nullerror"
      where fts = filter (\(a,b) -> a /= COMMENT ) ts

parseCmds :: [Token] -> ICmd
parseCmds ts = 
        case parse cmdP fts of
            Just (a,[]) -> a
            Just (a, n)  -> error $"parser error" ++ show n
            otherwise -> error "nullerror"
      where fts = filter (\(a,b) -> a /= COMMENT ) ts

programP :: Parser IDecl
programP = do

    name    <- trm PROGRAM *> ident
    --info "programP" name

    params  <- progParamsP
    --info "programP" params

    cps     <- trm GLOBAL *> optCps
    --info "programP" cps

    cmds    <- trm DO *> cpsCmdP <* trm ENDPROGRAM
    --info "programP" cmds

    return $IProg name params cps cmds

    where
        optCps :: Parser IDecl
        optCps = cpsDeclP <|> return INoDecl


cpsDeclP :: Parser IDecl
cpsDeclP = declP >>= repDecl
    where
        repDecl a = do
                b <- trm SEMICOLON *> declP
                -- info "cpsDeclP rep" b
                repDecl $IDeclItem a b
            <|> return a

cpsStoreDeclP :: Parser IDecl
cpsStoreDeclP = storeDeclP >>=  rep
    where
        rep a = do
                b <- trm SEMICOLON *> storeDeclP
                -- info "cpsStoreDeclP rep" b

                rep $IDeclItem a b
            <|> return a

declP :: Parser IDecl
declP = storeDeclP
    <|> funDeclP
    <|> procDeclP


funDeclP :: Parser IDecl
funDeclP = do
    i   <- trm FUN *> ident
    --info "funDeclP" i

    ps  <- paramsP
    --info "funDeclP" ps

    sd  <- trm RETURNS *> storeDeclP
    --info "funDeclP" sd

    gi  <- trm GLOBAL *> globImpsP <|> return INoParameter
    --info "funDeclP" gi
    
    li  <- trm LOCAL *> cpsStoreDeclP <|> return INoDecl
    --info "funDeclP" li
    
    cps <- trm DO *> cpsCmdP <* trm ENDFUN
    --info "funDeclP" cps

    return $IFunc i ps sd gi li cps


globImpsP :: Parser IParameter
globImpsP = globImpP >>= rep
    where rep a = do
            b <- trm COMMA *> globImpP
            --info "globImpsP" b

            rep $IProgParams a b
            <|> return a


globImpP :: Parser IParameter
globImpP = do
    fm <- trmA FLOWMODE     <|> return (FlowMode IN)
    --info "globImpP" fm

    cm <- trmA CHANGEMODE   <|> return (ChangeMode VAR)
    --info "globImpP" cm

    IGlobalImp fm cm <$> typedIdentP


storeDeclP :: Parser IDecl
storeDeclP = do
    cm <- trmA CHANGEMODE <|> return (ChangeMode VAR)
    --info "storeDeclP" cm

    IStore cm <$> typedIdentP


procDeclP :: Parser IDecl
procDeclP = do
    i    <- trm PROC *> ident
    ps   <- paramsP
    gi   <- trm GLOBAL *> globImpsP     <|> return INoParameter
    li   <- trm LOCAL *> cpsStoreDeclP  <|> return INoDecl
    cmds <- trm DO *> cpsCmdP <* trm ENDPROC
    return $IProc i ps gi li cmds

progParamsP :: Parser IParameter
progParamsP = do
        trm LPAREN
        p <- globImpsP
        trm RPAREN
        return p
    <|> do
        trm LPAREN
        trm RPAREN
        return INoParameter

        -- todo: make flowmode and mechmode optional


paramsP :: Parser IParameter
paramsP = do
    trm LPAREN
    p <- paramP >>= opti
    trm RPAREN
    return p

    where
        opti :: IParameter -> Parser IParameter
        opti a = do
                trm COMMA
                b <- paramP
                opti $IParams a b
                <|> return a

        -- todo: make flowmode and mechmode optional
        paramP :: Parser IParameter
        paramP = do
                fm <- trmA FLOWMODE   <|> return (FlowMode IN)
                mm <- trmA MECHMODE   <|> return (MechMode  REF)
                cm <- trmA CHANGEMODE <|> return (ChangeMode VAR)
                i <- typedIdentP
                return $IParam fm mm cm i


typedIdentP :: Parser IDecl
typedIdentP = typedIdentArrayP <|> do
        i <- ident
        t <- trm TYPEDEF *> trmA TYPE
        return $IType i t


typedIdentArrayP :: Parser IDecl
typedIdentArrayP =  do
        i <- ident
        t <- trm TYPEDEF *> trmA TYPE
        size <- opt <|> trm LEBRKT *> exprP <* trm REBRKT  
        return $IArrayType i size t
     where
         opt = do
            trm LEBRKT  
            trm REBRKT
            return INone

{- commands -}
cpsCmdP :: Parser ICmd
cpsCmdP = cmdP >>= opt
    where
        opt a = do
                b <- trm SEMICOLON *> cmdP
                opt $ICmds a b
            <|> return a

cmdP :: Parser ICmd
cmdP = do
        e <- trm IF *> exprP
        t <- trm THEN *> cpsCmdP
        f <- (trm ELSE *> cpsCmdP <|> return ISkip) <* trm ENDIF
        return (IIf e t f)

    <|> do
        e <- trm WHILE *> exprP
        c <- trm DO *> cpsCmdP <* trm ENDWHILE
        return $IWhile e c

    <|> do
        -- add globInits 
        s <- trm CALL *> ident
        e <- exprListP
        return $ICaller s e

    <|> do
        trm DEBUGIN
        IDebugIn <$> exprP

    <|> do
        trm DEBUGOUT
        IDebugOut <$> exprP

    <|> do
        a <- exprP
        b <- trm ASSIGN *> exprP
        return $IBecomes a b

{- expressions -}
exprP :: Parser IExpr
exprP = termRelP >>= exprP'
    where
        exprP' :: IExpr -> Parser IExpr
        exprP' a = do
            -- attr (LogicOperator OR)
            attr <- trmA LOGICOPR
            b <- termRelP
            c <- exprP' b

            return (IOpr attr c a)
            <|> return a


termRelP :: Parser IExpr
termRelP = termAddP >>= opt
    where
        opt a = do
            attr <- trmA RELOPR
            b <- termAddP
            return $IOpr attr a b
            <|> return a


termAddP :: Parser IExpr
termAddP = termMultP >>= opt
    where
        opt a  = do
            attr <- trmA ADDOPR
            b <- termMultP
            opt (IOpr attr a b)

            <|> return a


termMultP :: Parser IExpr
termMultP = factorP >>= opt
    where
        opt :: IExpr -> Parser IExpr
        opt a  = do
            attr <- trmA MULTOPR
            b <- factorP
            opt (IOpr attr a b)
            <|> return a

factorP :: Parser IExpr
factorP
    = IAliteral <$> digit
    <|> (ident >>= opt)

    <|> trm LPAREN *> exprP <* trm RPAREN
    <|> IMonadic <$> monadicOprP <*> factorP
    where opt i = do
                e <- exprListP
                return $IExprList i e

            <|> do
                s <- trm LEBRKT *> exprP <*trm REBRKT
                return $ILiteralArray i s 
            <|> 
                do
                trm DOT 
                trm LENGTH  

                return $IArrayLength i 
            <|> do
                trm INIT
                return $ILiteral i True
            <|> do return $ILiteral i False


monadicOprP :: Parser Attirbute
monadicOprP
    = trmAByAttr (LogicOperator NOT)
    <|> trmA ADDOPR


exprListP :: Parser IExpr
exprListP = do
        e <- trm LPAREN *> exprP
        n <- opt e <* trm RPAREN
        return n

    where
        opt a = do
             b <-  trm COMMA *> exprP
             opt (IExprListParams a b)
            <|> do return a

