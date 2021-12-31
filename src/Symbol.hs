module Symbol (
    Symbol,
    createSymbols,
    containsSymbol,
    getSymbol,
    initSymbols
) where



import Parser (IDecl(..), parseProgram)
import Scanner
import Debug.Trace

type Symbol = (String, IDecl, Bool)


getSymbol :: [Symbol] -> String -> Maybe Symbol
getSymbol [] _ = Nothing
getSymbol (a@(ids,_,_):ds) id
    | id == ids = Just a
    | otherwise = getSymbol ds id


{--contains symboltable a element with the given id--}
containsSymbol :: [Symbol] -> String -> Bool
containsSymbol [] _ = False
containsSymbol ((id, _, _):ds) idc
    | id == idc = True
    | otherwise = containsSymbol ds idc




initSymbols :: [Symbol] -> Bool -> String -> [Symbol]
initSymbols sym True id = map (`flipInit` id) sym
initSymbols sym _ _ = sym


flipInit :: Symbol -> String -> Symbol
flipInit a@(id,t@(IType _ _),_) ide
    | id == ide = (id, t, True)
    | id /= ide = a


createSymbols :: IDecl -> [Symbol]
createSymbols = addSymbols []

addSymbols :: [Symbol] -> IDecl -> [Symbol]
addSymbols sym (IProg _ _ glob _) = createSymbols' glob sym
    where
        createSymbols' (IDeclItem a b) sym                  = createSymbols' a (createSymbols' b sym)
        createSymbols' a@(IFunc id _ _ _ _ _) sym           = symbol (id) a sym : sym
        createSymbols' a@(IProc id _ _ _ _) sym             = symbol (id) a sym : sym
        createSymbols' a@(IStore _ (IType id _)) sym        = symbol id a sym: sym
        createSymbols' a@(IStore _ (IArrayType id _ _)) sym = symbol id a sym: sym
        createSymbols' _ _ = []

        symbol id a sym
            | not (containsSymbol sym id) = (id, a , False)
            | containsSymbol sym id = error $ "duplicated name '" ++ id ++ "'"



loadProg = do
    file <- readFile "../test/programs/array_sample.iml"
    let a = createSymbols (parseProgram (scanner file))

    return (a)
