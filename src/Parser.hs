{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser (
  IExpr(..),
  IParameter(..),
  IDecl(..)
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

data IParameter
    = INoParameter
    | IProgParams IParameter IParameter

    | IParams IParameter IParameter
    | IParam Attirbute Attirbute Attirbute IDecl

    | IGlobalImp Attirbute Attirbute IDecl
    | IStoreParams IParameter IDecl

    | ILocalImps IParameter IParameter

    deriving (Show)

data IDecl
    = IDeclItem IDecl IDecl
    | IStore Attirbute IDecl
    | IFunc String IParameter IDecl IParameter IDecl ICmd
    | IProc String IParameter IParameter IDecl ICmd
    | ICsp
    | IType String Attirbute
    | IArrayType String Int Attirbute 
    | IProg String IParameter IDecl ICmd
    | INoDecl
    deriving (Show)

data ICmd
    = IBecomes IExpr IExpr
    | IIf IExpr ICmd ICmd
    | ICmds ICmd ICmd
    | ISkip
    | IWhile IExpr ICmd
    | IDebugIn IExpr
    | IDebugOut IExpr
    | ICaller String IExpr
    deriving (Show)

data IExpr
    = IAliteal Int
    | ILiteral String Bool -- name , is init?
    | IMonadic Attirbute IExpr
    | IOpr Attirbute IExpr IExpr
    | IExprList String IExpr
    | IExprListParams IExpr IExpr
    | INone
    deriving ()

instance Show IExpr where
    show n = "\n\t" ++ show' n 2 ++ "\n"

        where
            show' :: IExpr -> Int -> String
            show' INone i           = "(INone)"
            show' (IAliteal n) i    = "(IAliteral " ++ show n ++ ")"
            show' (ILiteral n b) i  = "(ILiteral " ++ show n ++ " " ++ show b ++ ")"

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



{-testdaten-}
funCode = "fun fact1024(n:int32) returns var fact:int1024  do schinken init := 2 endfun"
procCode = "  proc euclidDivNat (in copy const a:int1024, out copy var numIt:int32)  do g := g*2 endproc "
cmdTest = "if a = b then // asdf \n a := 1 else a := 2;b := a endif; while x = y do x := y endwhile"
prog =   (scanner ("program Factorial (in n:int32) global " ++ funCode ++ "; " ++ procCode ++ " do " ++ cmdTest ++" endprogram"))


parseProgram :: [Token] -> IDecl 
parseProgram ts = case parse programP fts of
        Just (a,[]) -> a
        otherwise  -> error "parser error"
      where fts = filter (\(a,b) -> a /= COMMENT ) ts

programP :: Parser IDecl
programP = do
    name    <- trm PROGRAM *> ident
    params  <- progParamsP
    cps     <- trm GLOBAL *> optCps
    cmds    <- trm DO *> cpsCmdP <* trm ENDPROGRAM
    
    return $IProg name params cps cmds

    where
        optCps :: Parser IDecl
        optCps = cpsDeclP <|> return INoDecl


cpsDeclP :: Parser IDecl
cpsDeclP = declP >>= repDecl
    where
        repDecl a = do
                trm SEMICOLON
                b <- declP
                repDecl $IDeclItem a b
            <|> return a

cpsStoreDeclP :: Parser IDecl
cpsStoreDeclP = storeDeclP >>=  rep
    where 
        rep a = do
                b <- trm SEMICOLON *> storeDeclP
                rep $IDeclItem a b
            <|> return a

declP :: Parser IDecl
declP = storeDeclP
    <|> funDeclP
    <|> procDeclP


funDeclP :: Parser IDecl
funDeclP = do
    i   <- trm FUN       *> ident
    ps  <- paramsP
    sd  <- trm RETURNS   *> storeDeclP
    gi  <- trm GLOBAL    *> globImpsP        <|> return INoParameter
    li  <- trm LOCAL     *> cpsStoreDeclP    <|> return INoDecl
    cps <- trm DO *> cpsCmdP <* trm ENDFUN 

    return $IFunc i ps sd gi li cps


globImpsP :: Parser IParameter
globImpsP = globImpP >>= rep
    where rep a = do
                trm COMMA
                b <- globImpP
                rep $IProgParams a b
                <|> return a


globImpP :: Parser IParameter
globImpP = do
    fm <- trmA FLOWMODE     <|> return (FlowMode IN)
    cm <- trmA CHANGEMODE   <|> return (ChangeMode VAR)
    IGlobalImp fm cm <$> typedIdentP


storeDeclP :: Parser IDecl
storeDeclP = do
    cm <- trmA CHANGEMODE <|> return (ChangeMode VAR)
    IStore cm <$> typedIdentP


procDeclP :: Parser IDecl
procDeclP = do 
    i    <- trm PROC *> ident
    ps   <- paramsP
    gi   <- trm GLOBAL *> globImpsP <|> return INoParameter
    li   <- trm LOCAL *> cpsStoreDeclP <|> return INoDecl
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
typedIdentP = do
    i <- ident
    t <- trm TYPEDEF *> trmA TYPE
    return $IType i t




{- commands -}
cpsCmdP :: Parser ICmd
cpsCmdP = cmdP >>= opt
    where
        opt a = do
                trm SEMICOLON
                b <- cmdP

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
            return $IOpr attr b a
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
    = IAliteal <$> digit
    <|> do
        i <- ident
        opt i

    <|> trm LPAREN *> exprP <* trm RPAREN
    <|> IMonadic <$> monadicOprP <*> factorP
    where opt i = do
                e <- exprListP
                return $IExprList i e
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
             trm COMMA
             b <- exprP
             opt (IExprListParams a b)

            <|> do return a

