module TypeChecker (checkProgram, getExprType) where

import Scanner (scanner)
import Parser
import Model (Type (INT32, INT64, BOOLEAN),Terminal(IDENT, BOOL, FALSE), Attirbute (..), ChangeMode (VAR, CONST), AritmeticOperator (PLUS), RelOperator, FlowMode (IN), MechMode (REF, COPY))
import Data.Type.Coercion (sym)
import Symbol (Symbol, createSymbols, initSymbols, getSymbol, isInit, storeId, routineId, addSymbols, removeSymbols, setAddressById)
import Debug.Trace
import Utils.Logger
import Data.Void (Void)


testp = loadProg "../test/programs/p1.iml"
    where
        loadProg f = do
            file <- readFile f
            let pt = parseProgram (scanner file)
            let st = checkDecl [] pt
            
            return st


checkProgram :: IDecl -> [Symbol]
checkProgram = checkDecl [] 

checkDecl :: [Symbol] -> IDecl ->  [Symbol] 
checkDecl sym (IProg id _ glob cmd) = checkDecl sympass glob
    where 
        symglob = createSymbols glob id
        sympass = checkCmd symglob cmd
checkDecl sym (IDeclItem d1 d2) = sym2
    where
        sym2 = checkDecl (checkDecl sym d2) d1


checkDecl sym (IFunc id params ret glob loc cmd) = symf
    where
        sym1 = addSymbols sym  id (paramsToDelc params) 
        sym2 = addSymbols sym1 id ret   
        sym3 = addSymbols sym2 id loc  
        
        symp = checkCmd sym3 cmd
        symf = removeSymbols symp id 


checkDecl sym (IProc id params glob loc cmd) = symf
    where 
        sym1 = addSymbols sym  id (paramsToDelc params) 
        sym3 = addSymbols sym1 id loc  
        
        symp = checkCmd sym3 cmd
        symf = removeSymbols symp id 

checkDecl sym (IStore _ _) = sym
checkDecl sym b = error $"no patern match " ++ show b





checkCmd :: [Symbol] -> ICmd -> [Symbol]
checkCmd sym (ICmds a b)            = checkCmd (checkCmd sym a) b 
checkCmd sym (IBecomes a b)         = checkLValue (checkExprRValue sym b False) a
checkCmd sym (IIf expr cmd1 cmd2)   = 
    case checkExprRValue sym expr False of
        (sym1, VariableType BOOLEAN ,_) -> checkCmd (checkCmd sym1 cmd1) cmd2
        (sym1, n ,_) -> error $"[checkCmd] if expression type is <" ++ show  n ++ ">"    
    

checkCmd sym (IWhile expr cmd) = 
    case checkExprRValue sym expr False of
        (sym1, VariableType BOOLEAN ,_) -> checkCmd sym1 cmd
        (sym1, n ,_) -> error $"[checkCmd] while expression type is <" ++ show  n ++ ">"    

checkCmd sym (IDebugIn expr)  = checkLValue (sym, VariableType INT32 , False) expr  
checkCmd sym (IDebugOut expr) = let (a, _, _) = checkExprRValue sym expr False in a  

checkCmd sym (ICaller id expr) = 
    case getSymbol sym $routineId id of
        Just (_,id, (IProc _ params  _ _ _) , _, _) -> checkFunctionParams sym expr params
        Nothing -> error $"no procedure found with name " ++ id

checkCmd sym ISkip = sym 
checkCmd _ n = error $show n 



{-check if operator is valid-}
checkOperator :: [Symbol]  -> IExpr -> ([Symbol], Attirbute, Bool)
checkOperator sym (IOpr op a b) = checkOpr op
    where
        checkOpr (LogicOperator _)      = check (da == VariableType BOOLEAN && db == VariableType BOOLEAN) (symb, db, False)
        checkOpr (RelOperator _)        = check (da == db) (symb, VariableType BOOLEAN, False)
        checkOpr (AritmeticOperator _)  = check (da == db) (symb, db, False)
        checkOpr _                      = check (da == db) (symb, db, False)

        check True n = n
        check False _ = error $"\n\t[typecheck] no equal types "++ show a ++ show da ++ " "++ show b ++ show db ++ " opr" ++ show op

        isComparableOpr (LogicOperator _) = True 
        isComparableOpr (AritmeticOperator _) = True
        isComparableOpr (RelOperator _) = False 
        isComparableOpr _ = False 

        opflag = isComparableOpr op

        (syma, da, _) = checkExprRValue sym a opflag
        (symb, db, _) = checkExprRValue syma b opflag 

checkOperator a b = error $"\n\t[typecheck] no IOpr" ++ show b 




getExprType :: [Symbol] -> IExpr -> Attirbute
getExprType sym expr = let (_,a,_) = checkExprRValue sym expr False in a


{--
    check excpression r value 
    bool is for array ref or exclicit value
-}
checkExprRValue :: [Symbol] -> IExpr -> Bool -> ([Symbol], Attirbute, Bool)
checkExprRValue sym (b@IOpr {}) flag = checkOperator sym b 
checkExprRValue sym (ILiteralArray id expr) flag =
    case getSymbol sym $storeId id of
        Just (_,id,IStore _ (IArrayType _ _ decl), _, _) ->
            let (sn, _, _) = checkExprRValue sym expr flag
            in (sn, decl, False)
        _ -> error $"\n\t[typecheck] checkExprRValue var '" ++ id ++ "' not declared"

checkExprRValue sym (ILiteral id init) flag =
    case getSymbol sym $storeId id of
            Just (env ,id, IStore _ (IType _ decl), True , _)           -> (sym, decl, False) 
            Just (env ,id, IStore _ (IType _ decl), False  , _)         -> error $ "[typecheck] checkExprRValue var '" ++ id ++ "' not initialized in env " ++ env 
            Just (env ,id, IStore _ (IArrayType _ _ decl), True, _)     -> if not flag  then (sym, decl, True)  else  error $ "\n\t[typecheck] var array '" ++ id ++ "' is used in operation"
            Just (env ,id, IStore _ (IArrayType _ _ decl), False , _)   -> error $ "\n\t[typecheck] var array '" ++ id ++ "' not initialized in env"  ++ env
            Just (env, id, (IType _ decl), initialized, _) -> (sym, decl, False) 
            Just x -> error $show x
            _ -> error $"\n\t[typecheck] var '" ++ id ++ "' not declared:\n\t" ++  show sym

checkExprRValue sym (IAliteral a) flag = (sym, VariableType INT32, False) 
checkExprRValue sym (IArrayLength id ) flag = 
    case getSymbol sym $storeId id of
        Just _ -> (sym, VariableType INT32, False) 
        _ -> error $"\n\t[typecheck] var '" ++ id ++ "' not declared:\n\t" ++  show sym

checkExprRValue sym (IMonadic attr expr) flag = checkExprRValue sym expr True
checkExprRValue sym (IExprList id expr) flag =
    case getSymbol sym  $routineId id of
            Just (_,id, (IFunc _ params (IStore _ (IType _ attr)) _ _ _) , _, _) -> 
                let s = checkFunctionParams sym expr params
                in (s, attr, False)
            _ -> error $"\n\t[typecheck] function '" ++ id ++ "' does not exist"

checkExprRValue sym (IExprListParams a b) flag = error $"\n\t[typecheck] function has too many parameter '" ++ show a ++ show b ++"'" 
checkExprRValue sym a flag = error $"\n\t[typecheck] operation error "  ++ show a

{- function parameter check -}
checkFunctionParams :: [Symbol] -> IExpr -> IParameter -> [Symbol]
checkFunctionParams sym (IExprListParams e1 e2)  (IParams a b) = sym2
    where
        sym1 = checkFunctionParams sym e1 a
        sym2 = checkFunctionParams sym1 e2 b

checkFunctionParams sym expr (IParam _ _ _ (IType id attr)) = 
    let (s, attre, _) = checkExprRValue sym expr False
    in if attre == attr then s 
        else error $"\n\t[typecheck] function parameter does not expect type = " ++ show attr ++ " param = " ++ show attre

checkFunctionParams sym b n = error $"\n\t[typecheck] function has too view parameters "++ show b ++ show n



{- check if L Value is valid ... := {X} 
    checks const -}
checkLValue :: ([Symbol], Attirbute, Bool) -> IExpr -> [Symbol]
checkLValue (sym, attr, _) (ILiteralArray id  expr) = 
    case getSymbol sym $storeId id of
        Just (_,id, IStore _ (IArrayType _ _ attrl), ref, _) -> 
            if  attr == attrl 
                then sym 
                else error $"\n\t[typecheck] no equal type " ++ show attr ++ " not equal " ++ show attrl 

        Just val -> error $"\n\t[typecheck] no matching type " ++ show val  
        Nothing -> error $"\n\t[typecheck] no type defined for var '" ++ id ++ "'"

checkLValue (sym, attr, ref) (ILiteral id init) =
    let newSym = if isInit sym id && init
            then error $"const var '" ++show id ++ "' is  allready initialized"
            else initSymbols sym init $storeId id

    in case getSymbol newSym $storeId id of
            Just (_,id,IStore (ChangeMode cm) (IType _ decl), True, _) ->           
                if attr == decl 
                        then if cm == CONST && not init 
                            then error $"\n\t[typecheck] const var '" ++ show id ++ "' can not be changed after initialization" 
                            else newSym 
                        else error $ "\n\t[typecheck] l value var '" ++ id ++ "' is not from type " ++ show attr
            Just (_,id,IStore (ChangeMode cm) (IType _ decl), False , _) -> error $ "var '" ++ id ++ "' not initialized"
            Just (_,id,IStore _ (IArrayType _ _ decl), _, _) -> 
                if attr == decl && ref
                    then newSym 
                    else error $ "\n\t[typecheck] l value var '" ++ id ++ "' is not from type " ++ show attr

            n -> error $"var '" ++ id ++ "' not declared" ++ show n
checkLValue a n = error "\n\t[typecheck] no valid L value "

