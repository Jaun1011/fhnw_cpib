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
    getRoutineSymbol,
    getStoreSymbol,
    initSymbols,
    getType,
    getAddress,
    setAddressById
) where



import Parser (IDecl(..), parseProgram, IExpr (ILiteralArray))
import Scanner
import Debug.Trace
import Data.List (sortBy)
import Vm.VirtualMachineIOStudents (Instruction(Add))
import Model (Attirbute)

type Id = String
type Env = String
type Ref = Bool
type Address = Int

type Symbol = (Env, Id, IDecl, Ref, Address)

getType :: Symbol -> Maybe Attirbute
getType (_, _, IType _ attr, _, _) = Just attr
getType (_, _, IArrayType _ _ attr, _, _) = Just attr
getType _ = Nothing 

getAddress :: Symbol -> Address
getAddress (_, _, _, _, addr) =addr

getId :: Symbol -> Id
getId (_, id, _, _, _) = id


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



storeId :: String -> String
storeId  id = "store." ++ id

routineId :: String -> String
routineId  id = "routine." ++ id


getStoreSymbol :: [Symbol] -> String -> Maybe Symbol
getStoreSymbol sym id = getSymbol sym $storeId id


getRoutineSymbol :: [Symbol] -> String -> Maybe Symbol
getRoutineSymbol sym id = getSymbol sym $routineId id


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


addSymbols :: [Symbol] -> String -> IDecl -> [Symbol]
addSymbols sym env (IProg _ _ glob _) = addSymbols sym env glob
addSymbols sym env (IDeclItem a b) = addSymbols (addSymbols sym env a) env b
addSymbols sym env a = sortSymbols $createSymbols' a sym
    where
        createSymbols' a sym = symbol (getId a) a sym : sym
        createSymbols' _ _ = []

        symbol id a sym
            | not (containsSymbol sym id) = (env, id, a , False, -1)
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


