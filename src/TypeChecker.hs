import Scanner (scanner)
import Parser
import Model (Type (INT32, INT64, BOOLEAN),Terminal(IDENT, BOOL), Attirbute (..), ChangeMode (VAR), AritmeticOperator (PLUS), RelOperator)
import Data.Type.Coercion (sym)
import Symbol (Symbol, createSymbols, initSymbols, getSymbol)










{--

glob = [
    (id2, bool),
    (id1, int)
] 
f1 = [

]
1) liste für env generieren
2) cmds für env checken







typeCheck :: IDecl -> [IDecl] 
typeCheck (IProg id params glob loc) = typeCheck glob
typeCheck a = [a]
    where
        typecheck' :: IDecl -> [IDecl] -> [IDecl]
        typecheck' (IProg id params glob cmds) cs = typecheck' glob cs
        typecheck' (IFunc id params ret glob loc  cmds) cs = typecheck' 
        typecheck' _ _ = []

--}


testSym :: [Symbol]
testSym = [
    ("a", IStore (ChangeMode  VAR) $IType "a" (VariableType INT32),True),
    ("ab", IStore (ChangeMode  VAR) $IType "ab" (VariableType INT64),True),
    ("b", IStore (ChangeMode  VAR) $IArrayType  "b" (IAliteral 10) (VariableType INT32),True)
    ]

testExpr :: IExpr
testExpr = IOpr (AritmeticOperator PLUS) (IOpr (AritmeticOperator PLUS) INone (ILiteral "ab" False) ) (ILiteral "a" False) 


testExpr2 :: IExpr
testExpr2 = IOpr (AritmeticOperator PLUS) (IAliteral 1) (ILiteral "a" False) 




checkAst :: IDecl -> Bool 
checkAst (IProg _ _ glob cmd) = checkCmd types cmd
    where
        types = createSymbols glob



checkCmd :: [Symbol] -> ICmd -> Bool 
checkCmd (d:ds) (ICmds a b)            = True
checkCmd (d:ds) (IBecomes a b)         = True
checkCmd (d:ds) (IIf expr cmd1 cmd2)   = True
checkCmd (d:ds) (IWhile expr cmd)      = True
checkCmd (d:ds) (ICaller id expr)      = True
checkCmd (d:ds) (IDebugIn expr)        = True
checkCmd (d:ds) (IDebugOut expr)       = True



checkOperator :: [Symbol] -> IExpr -> ([Symbol], Attirbute)
checkOperator sym (IOpr op a b) = checkOpr op
    where
        checkOpr (LogicOperator _)      = check (da == VariableType BOOLEAN && db == VariableType BOOLEAN) (symb, db)
        checkOpr (RelOperator _)        = check (da == db) (symb, VariableType BOOLEAN)
        checkOpr (AritmeticOperator _)  = check (da == db) (symb, db)
        checkOpr _  = check (da == db) (symb, db)

        check True n = n 
        check False _ = error $"[typecheck] no equal types " ++ show da ++ " " ++ show db ++ " opr" ++ show op

        (syma, da) = checkExprLValue sym a
        (symb, db) = checkExprLValue syma b
checkOperator _ _ = error "no IOpr"



checkExprLValue :: [Symbol] -> IExpr -> ([Symbol], Attirbute)
checkExprLValue a b = checkOperator a b
checkExprLValue sym (ILiteralArray id expr) = 
    case getSymbol sym id of 
        Just (id,IStore _ (IArrayType _ _ decl), _) -> 
            let (sn, _) = checkExprLValue sym expr
            in (sn, decl)
        _ -> error $"var '" ++ id ++ "' not declared"

checkExprLValue sym (ILiteral id init) = 
    let newSym = initSymbols sym init id
    in case getSymbol newSym id of 
            Just (id,IStore _ (IType _ decl), int) ->
                if int 
                    then (newSym, decl) 
                    else error $ "var '" ++ id ++ "' not initialized" 
            _ -> error $"var '" ++ id ++ "' not declared"

checkExprLValue sym (IMonadic _ expr) = checkExprLValue sym expr
checkExprLValue sym (IExprList _ expr) = checkExprLValue sym expr
checkExprLValue sym (IExprListParams e1 e2) = r
    where 
        (s1, _) = checkExprLValue sym e1
        r = checkExprLValue s1 e2

checkExprLValue sym (IAliteral _) =  (sym, VariableType INT32)
checkExprLValue sym INone =  (sym, VariableType INT32)

checkExprLValue sym n = error $"undefined expression type" ++ show n
checkExprRValue :: [Symbol] -> IExpr -> [Symbol]
checkExprRValue sym expr = []






loadProg = do
    file <- readFile "../test/programs/array_sample.iml"
    
    let val = parseProgram (scanner file)
    writeFile "./test.hs" (show val)
    return val







fastparse value = createSymbols $parseProgram (scanner value)