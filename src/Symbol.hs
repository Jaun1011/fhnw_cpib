module Symbol (
    Symbol,
    ProgScope(..),
    storeId,
    routineId,
    prefixId,
    removeSymbols,
    isInit,
    createSymbols,
    addSymbols,
    containsSymbol,
    getSymbol,
    findRoutineSymbol,
    findStoreSymbol,
    initSymbols,
    getType,
    getAddress,
    setAddressById,
    addSymbolsImpl,
    routineEnvId,
    storeEnvId
) where



import Parser (IDecl(..), parseProgram, IExpr (ILiteralArray))
import Debug.Trace
import Data.List (sortBy)
import Vm.VirtualMachineIOStudents (Instruction(Add))
import Model (Attirbute, Terminal (GLOBAL))
import Vm.BaseDecls (Scope(Global))

type Id = String
type Env = String
type Ref = Bool
type Address = Int

type Symbol = (Env, Id, IDecl, Ref, Address)
data ProgScope 
    = Glob
    | Local
    deriving(Show)


getType :: Symbol -> Maybe Attirbute
getType (_, _, IType _ attr, _, _) = Just attr
getType (_, _, IArrayType _ _ attr, _, _) = Just attr
getType _ = Nothing

getAddress :: Symbol -> Address
getAddress (_, _, _, _, addr) =addr

getId :: Symbol -> Id
getId (_, id, _, _, _) = id


getDecl :: Symbol -> IDecl 
getDecl (_, _, decl, _, _) = decl


setAddressById :: [Symbol] -> (Address, Id) -> [Symbol]
setAddressById sym n = map (`change` n) sym
        where
            change :: Symbol -> (Address, Id)  -> Symbol
            change (env, id, t, val, addr) (addrnew, ide)
                    | id == ide = (env, id, t, val, addrnew)
            change a n = a

sortSymbols :: [Symbol] -> [Symbol]
sortSymbols = sortBy orderSym


orderSym :: Symbol -> Symbol -> Ordering
orderSym (_, _, IStore {}, _,_) (_, _, IFunc {}, _, _) = LT
orderSym (_, _, IStore {}, _,_) (_, _, IProc {}, _,_) = LT
orderSym _ _ = EQ


findStoreSymbol :: [Symbol] -> Env -> Id -> (Symbol, ProgScope)
findStoreSymbol sym env id =  case getSymbol sym (storeId id) of 
        Just n -> (n, Glob)
        Nothing  -> case getSymbol sym (storeEnvId env id) of
                Just g -> (g, Local)
                Nothing -> error $"nothing found with ID <"++ (storeEnvId env id) ++ "> in scopes [\"\", " ++ env ++ "\"]\n\n" ++ show sym

findRoutineSymbol :: [Symbol] -> String -> Maybe Symbol
findRoutineSymbol sym id = getSymbol sym $routineId id


storeId :: String -> String
storeId  id = "store." ++ id

routineId :: String -> String
routineId  id = "routine." ++ id

routineEnvId :: String -> String -> String
routineEnvId pre id = routineId (prefixId pre id)

storeEnvId :: String -> String -> String
storeEnvId pre id = storeId (prefixId pre id)

prefixId :: [Char] -> [Char] -> [Char]

prefixId "" id = id
prefixId pre id = pre ++ "." ++ id








getSymbol :: [Symbol] -> String -> Maybe Symbol
getSymbol [] _ = Nothing
getSymbol (a@(env, ids,_,_,_):ds) id
    | id == ids = Just a
    | otherwise = getSymbol ds id


{--contains symboltable a element with the given id--}
containsSymbol :: [Symbol] -> String -> Bool
containsSymbol [] _ = False
containsSymbol ((env, id, _, _,_):ds) idc
    | id == idc = True
    | otherwise = containsSymbol ds idc


initSymbols :: [Symbol] -> Bool -> String -> [Symbol]
initSymbols sym True id = map (`flipInit` id) sym
initSymbols sym _ _ = sym


flipInit :: Symbol -> String -> Symbol
flipInit a@(env, id,t, val, addr) ide
        | id == ide = (env, id, t, True, addr)
        | id /= ide = a

isInit :: [Symbol] -> String -> Bool
isInit [] _ = False
isInit ((env, idi, _,i,_):ds) id
    | id == idi = i
    | otherwise  = isInit ds id



createSymbols :: IDecl -> String -> [Symbol]
createSymbols decl env = addSymbols [] env decl

removeSymbols :: [Symbol] -> String -> [Symbol]
removeSymbols [] _ = []
removeSymbols (d@(e,_,_,_,_):ds) env
    | e == env = removeSymbols ds env
    | otherwise = d :  removeSymbols ds env


addSymbolsImpl :: [Symbol] -> String -> String -> IDecl -> [Symbol]
addSymbolsImpl sym env pref (IProg _ _ glob _)  = addSymbolsImpl sym env pref glob
addSymbolsImpl sym env pref (IDeclItem a b) = addSymbolsImpl (addSymbols sym env a) env pref b
addSymbolsImpl sym env prefix a = sortSymbols $createSymbols' a sym
    where
        createSymbols' a sym
            | getId a /= "" = symbol (getId a) a sym : sym
            | getId a == "" = sym
       
       
        createSymbols' _ _ = []

        symbol id a sym
            | not (containsSymbol sym id) = (env, id, a , False, -99999999)
            | containsSymbol sym id = error $ "\n\n[symbol] duplicated name '" ++ id ++ "'\n\n" ++ show sym


        getId :: IDecl -> String
        getId (IFunc id _ _ _ _ _)           = routineId $prefixId prefix id
        getId (IProc id _ _ _ _)             = routineId $prefixId prefix id
        getId (IStore _ (IType id _))        = storeId $prefixId prefix  id
        getId (IStore _ (IArrayType id _ _)) = storeId $prefixId prefix  id
        getId (IType id _)                   = storeId $prefixId prefix  id
        getId INoDecl                        = ""
        getId t = error $show t
       

addSymbolsImpl a b c d = error $ show a ++ show b ++ show c


addSymbols :: [Symbol] -> String -> IDecl -> [Symbol]
addSymbols sym env decl = addSymbolsImpl sym env "" decl