{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Model
import Scanner (scanner)
import Model
import Control.Applicative
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
item = P (next id)


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
trm t = do
    (tc, a) <- item

    if tc == t
        then return (tc, a)
        else empty

trmA :: Terminal  -> Parser Attirbute
trmA t = do
    (_, attr) <- trm t
    case attr of
        Just a -> return a
        _ -> empty

trmAByAttr :: Attirbute  -> Parser Attirbute
trmAByAttr a = do
    (_, ac) <- item
    case ac of
        Just ac ->
            if a == ac
            then return ac
            else empty

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
    | IProc IParameter
    | ICsp
    | IType String Attirbute
    | IProg String IParameter IDecl ICmd
    | INoDecl
    deriving (Show)


programP :: Parser IDecl
programP = do
    trm PROGRAM
    name <- ident
    params <- progParamsP
    cps <- optCps

    trm DO
    cmds <- cpsCmdP
    trm ENDPROGRAM
    return $IProg name params cps cmds

    where
        optCps :: Parser IDecl
        optCps = do
            trm GLOBAL
            cpsDeclP
           <|> return INoDecl


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
procDeclP = do return INoDecl

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
    trm TYPEDEF
    t <- trmA TYPE
    return $IType i t


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


cmdP :: Parser ICmd
cmdP = IBecomes <$> exprP <* trm ASSIGN <*> exprP
    <|> do
        trm IF
        e <- exprP
        trm THEN
        c1 <- cpsCmdP
        trm ELSE
        IIf e c1 <$> cpsCmdP

    <|> do
        trm IF
        e <- exprP
        trm THEN
        c1 <- cpsCmdP
        return $IIf e c1 ISkip
    <|> do
        trm WHILE
        e <- exprP

        trm DO
        c <- cpsCmdP

        trm ENDWHILE
        return $IWhile e c

    <|> do
        -- add globInits 
        trm CALL
        s <- ident
        e <- exprListP

        return $ICaller s e

    <|> do
        trm DEBUGIN
        IDebugIn <$> exprP

    <|> do
        trm DEBUGOUT
        IDebugOut <$> exprP

cpsCmdP :: Parser ICmd
cpsCmdP = (cmdP >>= opt)  <|>  cmdP
    where
        opt c1 = do
                trm SEMICOLON
                c2 <- cmdP
                opt $ICmds c1 c2


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
            <|> do return a


termRelP :: Parser IExpr
termRelP = opt termAddP
    where
        opt a = IOpr <$> trmA RELOPR <*> termAddP <*> a
             <|> a


termAddP :: Parser IExpr
termAddP = termMultP >>= opt
    where
        opt a  = do
            attr <- trmA ADDOPR
            b <- termMultP
            opt (IOpr attr a b)

            <|> do return a


termMultP :: Parser IExpr
termMultP = factorP >>= opt
    where
        opt :: IExpr -> Parser IExpr
        opt a  = do
            attr <- trmA MULTOPR
            b <- factorP
            opt (IOpr attr a b)

            <|> do return a

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
        _ <- trm LPAREN
        e <- exprP
        n <- opt e
        _ <- trm RPAREN
        return n

    where
        opt a = do
             trm COMMA
             b <- exprP
             opt (IExprListParams a b)

            <|> do return a

