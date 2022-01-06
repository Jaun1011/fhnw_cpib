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
import Parser (IDecl (IProg, IDeclItem, IStore, IType, IFunc, IProc, INoDecl, IArrayType), parseProgram, IExpr (IOpr, IAliteral, ILiteral), ICmd (ISkip, IBecomes, ICmds, IDebugOut, IDebugIn, IIf, IWhile), IParameter (IParams, IParam, INoParameter), paramsToDelc)
import Model (Attirbute(AritmeticOperator, RelOperator), AritmeticOperator (PLUS, MINUS, MULTI, DIV), Terminal (RELOPR), RelOperator (EQUAL, GREATER, LESS, LESS_EQUAL, GREATER_EQUAL, NOT_EQUAL))
import Debug.Trace
import qualified Vm.BaseDecls as Model
import Text.Read (Lexeme(String))


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
            cmdins = cmdC symalc cmd (length allins)
            
            
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

                alloc sym decl@(IFunc id params ret@(IStore _ d) glob loc cmd) (st,e,p,f) = 
                                (totinstr
                                ,symf6
                                ,(st,e, pt ,f))
                        where 
                            symf0 = setAddressById sym (p, routineId id)    

                            symf1 = addSymbols symf0 id (paramsToDelc params) 
                            symf2 = addSymbols symf1 id ret   
                            symf3 = addSymbols symf2 id loc  

                            (symf4, i) = paramsC symf3 params 
                            symf5 = setAddressById symf4 (i,storeId $literalId d) 
                            
                            (instLoc, symf6, store) = allocC symf5 loc 
                            instCmd = cmdC symf6 cmd pc
             

                            totinstr = [UncondJump (pt + 2)]  ++ instLoc ++ instCmd ++ [Return 1]

                            pc = p + length instLoc
                            pt = pc + length instCmd

                alloc sym t c = error $show t





literalId :: IDecl -> String
literalId (IArrayType i _ _) = i
literalId (IType i _)        = i 
literalId _ = error "no type"

declAllocC :: [Symbol]  -> (IParameter,Int) -> ([Symbol], Int)
declAllocC sym (IParams  a b, addr) = declAllocC syma (b, addra) 
                
                        where  (syma, addra) = declAllocC sym (a, addr) 

declAllocC sym (IParam _ _ _ lit, addr) = (setAddressById sym (addr, literalId lit), addr - 1)

                

paramsC :: [Symbol]  -> IParameter -> ([Symbol], Int)
paramsC sym params = declAllocC sym (params, -1)




cmdC :: [Symbol] -> ICmd -> Int -> [Instruction]
cmdC sym (ICmds a b)  pc = (le ++ ri) 
        where 
                le = cmdC sym a pc
                ri = cmdC sym b (pc + length le)
cmdC sym ISkip  pc = []
cmdC sym (IBecomes l r)  pc =  oprL sym l  ++ oprR sym r ++ [Store]
cmdC sym (IIf expr cmd1 cmd2)  pc = inst
        where 
                addrIf = pc + length expri + length ifi + 2
                addrElse  = addrIf + length eli

                expri = oprR sym expr
                ifi = cmdC sym cmd1 (pc + length expri + 1)
                eli = cmdC sym cmd2 addrIf
                
                inst = expri ++ [CondJump addrIf] ++ ifi ++ [UncondJump addrElse] ++ eli 

cmdC sym (IWhile expr cmd) pc = inst
        where
                addrStart = pc
                addrEnd   = pc + length expri + length loop + 2

                expri   = oprR sym expr 
                loop    = cmdC sym cmd  (pc + length expri + 1) 

                inst = expri ++ [CondJump addrEnd] ++ loop ++ [UncondJump addrStart]


cmdC sym (IDebugIn expr)  pc = oprL sym expr  ++ [Input (IntTy WL1024) (Loc Nothing) ""]
cmdC sym (IDebugOut expr)  pc = oprR sym expr  ++ [Output (IntTy WL1024) ""]


cmdC sym b pc =  error $"cmdC " ++show b


addrerssByID sym id = case getSymbol sym id of 
        Just a -> getAddress a
        Nothing -> error $"\n\nno address found " ++ id ++ " in sym table\n\n"++ show sym




oprL :: [Symbol] -> IExpr -> [Instruction]
oprL sym (ILiteral id _) = case getStoreSymbol sym id of
        Just (_,_,IStore {},_,addr) -> 
                if addr >= 0 
                        then [LoadIm IntVmTy (IntVmVal addr)]
                        else [LoadAddrRel addr]
        
oprL sym _  =  error $"no l value"


oprR :: [Symbol] -> IExpr -> [Instruction]
oprR sym (IOpr o e1 e2) = conc $opr' o
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

        conc op = oprR sym e1  ++ oprR sym e2  ++ op

oprR sym (IAliteral val) = [LoadIm Int1024VmTy (Int1024VmVal $Int1024 $toInteger val)]
oprR sym (ILiteral id _)   = inc
        
        where inc = case getStoreSymbol sym id of 
                Just (_,_,IStore {},_,addr) -> [LoadIm IntVmTy (IntVmVal addr) ,Deref]
                Just (_,_,IType {},_,addr)  -> [LoadAddrRel addr, Deref]
                
                Just x -> error $"no valid label found " ++ show x
                Nothing -> error $"no value found "++ show id ++ " " ++ show sym 



oprR sym n  = error $"no supported opr " ++ show n

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
    


