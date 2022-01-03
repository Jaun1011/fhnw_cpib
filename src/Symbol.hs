module Symbol (
    Symbol,
    storeId,
    routineId,
    removeSymbols,
    isInit,
    createSymbols,
    addSymbols,
    containsSymbol,
    getSymbol,
    initSymbols,
) where



import Parser (IDecl(..), parseProgram, IExpr (ILiteralArray))
import Scanner
import Debug.Trace

type Env = String 
type Ref = Bool 
type Symbol = (String, Env, IDecl, Ref)



storeId :: String -> String
storeId  id = "store." ++ id

routineId :: String -> String
routineId  id = "routine." ++ id


{--
containsInScope :: Scope -> String -> Bool 
containsInScope (loc, glob) = containsSymbol (loc ++ glob)
--}


getSymbol :: [Symbol] -> String -> Maybe Symbol
getSymbol [] _ = Nothing
getSymbol (a@(env, ids,_,_):ds) id
    | id == ids = Just a
    | otherwise = getSymbol ds id


{--contains symboltable a element with the given id--}
containsSymbol :: [Symbol] -> String -> Bool
containsSymbol [] _ = False
containsSymbol ((env, id, _, _):ds) idc
    | id == idc = True
    | otherwise = containsSymbol ds idc


initSymbols :: [Symbol] -> Bool -> String -> [Symbol]
initSymbols sym True id = map (`flipInit` id) sym
initSymbols sym _ _ = sym


flipInit :: Symbol -> String -> Symbol
flipInit a@(env, id,t, val) ide
    | id == ide = (env, id, t, True)
    | id /= ide = a

isInit :: [Symbol] -> String -> Bool  
isInit [] _ = False
isInit ((env, idi, _,i ):ds) id 
    | id == idi = i
    | otherwise  = isInit ds id



createSymbols :: IDecl -> String -> [Symbol]
createSymbols decl env = addSymbols [] env decl



removeSymbols :: [Symbol] -> String -> [Symbol]
removeSymbols [] _ = []
removeSymbols (d@(e,_,_,_):ds) env 
    | e == env = removeSymbols ds env
    | otherwise = d :  removeSymbols ds env


addSymbols :: [Symbol] -> String -> IDecl -> [Symbol]
addSymbols sym env (IProg _ _ glob _) = addSymbols sym env glob 
addSymbols sym env (IDeclItem a b) = addSymbols (addSymbols sym env a) env b
addSymbols sym env a = createSymbols' a sym
    where
        createSymbols' a sym = symbol (getId a) a sym : sym
        createSymbols' _ _ = []

        symbol id a sym
            | not (containsSymbol sym id) = (env, id, a , False)
            | containsSymbol sym id = error $ "[symbol] duplicated name '" ++ id ++ "'" ++ show sym


        getId :: IDecl -> String 
        getId (IFunc id _ _ _ _ _)           = routineId id
        getId (IProc id _ _ _ _)             = routineId id
        getId (IStore _ (IType id _))        = storeId id
        getId (IStore _ (IArrayType id _ _)) = storeId id
        getId (IType id _)                   = storeId id 
        getId INoDecl                        = "" 
        getId t = error $show t

addSymbols a b c = error $ show a ++ show b ++ show c


