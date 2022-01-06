module CodeGenerator () where


import Vm.VirtualMachineIOStudents(
    VMProgram(VMProgram),
    VmType(..),
    VmValue(..),
    Instruction(..),
    CodeAddress, StoreAddress, Code,
    execProgram, debugProgram)

import Data.Array ( Array, assocs, bounds, listArray )
import Vm.BaseDecls (Ident(Ident), Type (IntTy), WordLength (WL1024, WL32), IntValue (Int32Val, Int1024Val), Operator (Greater))
import Utils.Logger (info)
import Vm.CheckedArithmetic
import Vm.Locations (BaseLocation(Loc1, Loc0), Location (Loc))


import Symbol (createSymbols, Symbol, getSymbol, getAddress, setAddressById, storeId, getStoreSymbol, routineId, addSymbols, removeSymbols)
import TypeChecker
import Scanner (scanner)
import Parser (IDecl (IProg, IDeclItem, IStore, IType, IFunc, IProc, INoDecl), parseProgram, IExpr (IOpr, IAliteral, ILiteral), ICmd (ISkip, IBecomes, ICmds, IDebugOut, IDebugIn, IIf, IWhile), IParameter (IParams, IParam, INoParameter), paramsToDelc)
import Model (Attirbute(AritmeticOperator, RelOperator), AritmeticOperator (PLUS, MINUS, MULTI, DIV), Terminal (RELOPR), RelOperator (EQUAL, GREATER, LESS, LESS_EQUAL, GREATER_EQUAL, NOT_EQUAL))
import Debug.Trace
import qualified Vm.BaseDecls as Model


type StackPointer   = Int
type ExtremePointer = Int
type ProgramPointer    = Int
type FramePointer   = Int

type Storage = (StackPointer, ExtremePointer, ProgramPointer, FramePointer)



genCode :: IDecl -> ([Instruction], [Symbol])
genCode (IProg id _ glob cmd) = (allins ++ cmdins ++ [Stop], symalc)
    where 
            sym = createSymbols glob id
            (allins, symalc, st) = allocC sym glob
            cmdins = cmdC symalc cmd False (length allins)
            
            
genCode (IDeclItem a b) = ([],[])
genCode (IStore  _ _) =  ([],[])
genCode _ =  ([],[])







allocC ::  [Symbol]  -> IDecl -> ([Instruction], [Symbol], Storage)
allocC sym decl =  alloc sym decl (0,0,0,0)
        where 
                alloc sym (IDeclItem a b) store = (ia ++ ib,  symb, st2)
                                where 
                                        (ia, syma, st1) = alloc sym a store
                                        (ib, symb, st2) = alloc syma b st1
                alloc sym INoDecl t = ([],sym,t)
                alloc sym (IStore _ (IType id _)) (st,e,p,f) = 
                                ([AllocBlock 1]
                                ,setAddressById sym (st, storeId id)
                                ,(st + 1,e, p + 1,f))

                alloc sym decl@(IFunc id params ret glob loc cmd) (st,e,p,f) = 
                                (totinstr
                                ,removeSymbols symf id
                                ,(st,e, pt ,f))
                        where 
                            symf0 = setAddressById sym (p, routineId id)    

                            symf1 = addSymbols symf0 id (paramsToDelc params) 
                            symf2 = addSymbols symf1 id ret   
                            symf = addSymbols symf2 id loc  

                            paramsi = paramsC params 
                            (loci,symloc,_) = allocC symf loc 
                            cmdi = cmdC symloc cmd True pc 
             

                            totinstr = [UncondJump (pt) ]  ++ paramsi ++ loci ++ cmdi ++ [Return 1]


                            pc = p + length loci
                            pt = pc + length totinstr

                alloc sym t c = error $show t




paramsC :: IParameter -> [Instruction]
paramsC param = genCode (count param - 1) 
        where
                count (IParams  a b)  = count a + count b   
                count (IParam   _ _ _ _)  = -1
                count (INoParameter) = 0

                genCode :: Int -> [Instruction]
                genCode i 
                        | i < 0 = LoadAddrRel i : genCode (i + 1) 
                        | otherwise = []





cmdC :: [Symbol] -> ICmd-> Bool -> Int -> [Instruction]
cmdC sym (ICmds a b) isFunc pc = (le ++ ri) 
        where 
                le = cmdC sym a isFunc pc
                ri = cmdC sym b isFunc (pc + length le)
cmdC sym ISkip isFunc pc = []
cmdC sym (IBecomes l r) isFunc pc =  oprL sym l isFunc ++ oprR sym r isFunc++ [Store]
cmdC sym (IIf expr cmd1 cmd2) isFunc pc = inst
        where 
                addrIf = pc + length expri + length ifi + 2
                addrElse  = addrIf + length eli

                expri = oprR sym expr isFunc
                ifi = cmdC sym cmd1 isFunc (pc + length expri + 1)
                eli = cmdC sym cmd2 isFunc addrIf
                
                inst = expri ++ [CondJump addrIf] ++ ifi ++ [UncondJump addrElse] ++ eli 

cmdC sym (IWhile expr cmd) isFunc pc = inst
        where
                addrStart = pc
                addrEnd   = pc + length expri + length loop + 2

                expri   = oprR sym expr isFunc
                loop    = cmdC sym cmd isFunc (pc + length expri + 1) 

                inst = expri ++ [CondJump addrEnd] ++ loop ++ [UncondJump addrStart]


cmdC sym (IDebugIn expr) isFunc pc = oprL sym expr isFunc ++ [Input (IntTy WL1024) (Loc Nothing) ""]
cmdC sym (IDebugOut expr) isFunc  pc = oprR sym expr isFunc ++ [Output (IntTy WL1024) ""]


cmdC sym b isFunc pc =  error $"cmdC " ++show b


addrerssByID sym id = case getSymbol sym id of 
        Just a -> getAddress a
        Nothing -> error $"\n\nno address found " ++ id ++ " in sym table\n\n"++ show sym




oprL :: [Symbol] -> IExpr -> Bool -> [Instruction]
oprL sym (ILiteral id _) False = [LoadIm IntVmTy (IntVmVal (addrerssByID sym $storeId id))]
oprL sym (ILiteral id _) True = []
oprL sym _ _ =  error $"no l value"


oprR :: [Symbol] -> IExpr -> Bool -> [Instruction]
oprR sym (IOpr o e1 e2) isFunc = conc $opr' o
     where
        opr' (AritmeticOperator PLUS)    = [Add Int1024VmTy (Loc Nothing)]
        opr' (AritmeticOperator MINUS )  = [Sub Int1024VmTy (Loc Nothing)]
        opr' (AritmeticOperator MULTI )  = [Mult Int1024VmTy (Loc Nothing)]
        opr' (AritmeticOperator DIV )    = [DivEuclid Int1024VmTy (Loc Nothing)]
        
        opr' (RelOperator EQUAL)          = [Eq Int1024VmTy]
        opr' (RelOperator NOT_EQUAL)      = [Ne Int1024VmTy]
        opr' (RelOperator GREATER)        = [Gt Int1024VmTy]
        opr' (RelOperator GREATER_EQUAL)  = [Ge Int1024VmTy]
        opr' (RelOperator LESS)           = [Lt Int1024VmTy]
        opr' (RelOperator LESS_EQUAL)     = [Le Int1024VmTy]

        opr' t = error $show t

        conc op = oprR sym e1 isFunc ++ oprR sym e2 isFunc ++ op

oprR sym (IAliteral val) isFunc = [LoadIm Int1024VmTy (Int1024VmVal $Int1024 $toInteger val)]
oprR sym (ILiteral id _) True = [Deref]
oprR sym (ILiteral id _) False  = [LoadIm IntVmTy (IntVmVal (getAddress s)) ,Deref]
        where s= case getStoreSymbol sym id of 
                Just x -> x -- todo check function
                Nothing -> error $"no value found "++ show id ++ " " ++ show sym 



oprR sym n _ = error $"no supported opr " ++ show n





runProg = do
        (e,l) <- testProg

        let n = listArray (0, length e - 1) e

        let prog = VMProgram (Ident "asdf",  n)
        debugProgram prog






testProg = do
    --file <- readFile "../test/programs/array_sample.iml"
    file <- readFile "../test/programs/p1.iml"

    let val = parseProgram  (scanner file)
    let check = checkProgram val

    return $genCode val


