import Scanner (scanner)
import Parser
import Model (Terminal(IDENT, BOOL))
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


checkExprType :: [Symbol] -> IExpr -> [Symbol]
checkExprType sym@(d:ds) (ILiteralArray id expr) = []
checkExprType sym@(d:ds) (ILiteral id init) = 
    let newSym = initSymbols sym init id
    in case getSymbol newSym id of 
            Just (id,IStore _ _, int) -> if int 
                    then newSym 
                    else error $"'" ++ id ++ "' not initialized" 
            _ -> error $"type '" ++ id ++ "' not declared"
checkExprType _ _ = []


checkExprType a@(d:ds) (IExprList e1 e2) = []








loadProg = do
    file <- readFile "../test/programs/array_sample.iml"
    return $createSymbols $parseProgram (scanner file)







