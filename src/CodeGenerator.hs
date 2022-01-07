module CodeGenerator () where


import Vm.VirtualMachineIOStudents(
    VMProgram(VMProgram),
    VmType(..),
    VmValue(..),
    Instruction(..),
    CodeAddress, StoreAddress, Code,
    execProgram, debugProgram)

import Data.Array ( Array, assocs, bounds, listArray, Ix (index) )
import Vm.BaseDecls (Ident(Ident), Type (IntTy), WordLength (WL1024, WL32), IntValue (Int32Val, Int1024Val), Operator (Greater))
import Utils.Logger (info)
import Vm.CheckedArithmetic
import Vm.Locations (BaseLocation(Loc1, Loc0), Location (Loc))


import Symbol (createSymbols, Symbol, getSymbol, getAddress, setAddressById, storeId, routineId, addSymbols, removeSymbols, addSymbolsImpl, storeEnvId, routineEnvId, findStoreSymbol, findRoutineSymbol)
import TypeChecker
import Scanner (scanner)
import Parser (IDecl (IProg, IDeclItem, IStore, IType, IFunc, IProc, INoDecl, IArrayType), parseProgram, IExpr (IOpr, IAliteral, ILiteral, IExprList, IExprListParams, ILiteralArray, IArrayLength), ICmd (ISkip, IBecomes, ICmds, IDebugOut, IDebugIn, IIf, IWhile), IParameter (IParams, IParam, INoParameter), paramsToDelc)
import Model (Attirbute(AritmeticOperator, RelOperator), AritmeticOperator (PLUS, MINUS, MULTI, DIV), Terminal (RELOPR), RelOperator (EQUAL, GREATER, LESS, LESS_EQUAL, GREATER_EQUAL, NOT_EQUAL))
import Debug.Trace
import qualified Vm.BaseDecls as Model
import Text.Read (Lexeme(String))
import GHC.Real (reduce)


type Env = String
type StackPointer   = Int
type ExtremePointer = Int
type ProgramPointer    = Int
type FramePointer   = Int

type Storage = (StackPointer, ExtremePointer, ProgramPointer, FramePointer)

addProgPointer :: Storage -> [a] -> Storage
addProgPointer (a,b,c,d) c2 = (a,b,c + length c2,d)


genCode :: IDecl -> ([Instruction], [Symbol])
genCode (IProg id _ glob cmd) = (allins ++ cmdins ++ [Stop], symalc)
    where 
            sym = createSymbols glob id
            (allins, symalc, st) = allocC sym env glob
            cmdins = cmdC symalc env st cmd  (length allins)

            env = ""

genCode (IDeclItem a b) = ([],[])
genCode (IStore  _ _) =  ([],[])
genCode _ =  ([],[])


allocC ::  [Symbol] -> Env -> IDecl -> ([Instruction], [Symbol], Storage)
allocC sym env decl =  alloc sym decl (0,0,0,0)
        where 
                alloc sym (IDeclItem a b) store = (ia ++ ib,  symb, st2)
                                where 
                                        (ia, syma, st1) = alloc sym a store
                                        (ib, symb, st2) = alloc syma b st1
                alloc sym INoDecl t = ([],sym,t)
                
                alloc sym (IStore _ (IArrayType id expr _)) (st,e,p,f) = 
                                (
                                 instExp ++ instComp
                                ,setAddressById sym (st + 1, storeId id)
                                ,(st + maxarraysize + 1,e, p + 1,f))

                    where 
                        addrLen   = st 
                        addrArray = addrLen + 1

                        instExp = [AllocBlock 1
                                  ,LoadIm IntVmTy (IntVmVal addrLen)] 
                                  
                                  ++ oprR sym env (p + 2) expr 
                                  ++ [Store]
                                  ++ [LoadIm IntVmTy (IntVmVal addrLen)
                                     ,Deref]


                        instComp = [LoadIm Int1024VmTy (Int1024VmVal $Int1024 $toInteger maxarraysize)
                                   ,Gt Int1024VmTy
                                   ,CondJump $sizeCond - 1
                                   ,Output (IntTy WL1024) $"[Runtime Exception] array is bigger than maxsize = " ++ show maxarraysize
                                   ,Stop
                                   ,AllocBlock $maxarraysize
                                   ]

                        sizeCond = p + length instExp  + length instComp
                        maxarraysize = 10

                alloc sym (IStore _ (IType id _)) (st,e,p,f) = 
                                ([AllocBlock 1]
                                ,setAddressById sym (st, storeId id)
                                ,(st + 1,e, p + 1,f))

                alloc sym decl@(IFunc id params ret@(IStore _ d) glob loc cmd) (st,e,p,f) = 
                                (totinstr
                                ,symf6
                                ,(st,e, pt + 1,f))
                        where 
                            symf0 = setAddressById sym (p + 1, routineEnvId env id)    

                            symf1 = addSymbolsImpl symf0 id id (paramsToDelc params) 
                            symf2 = addSymbolsImpl symf1 id id ret   
                            symf3 = addSymbolsImpl symf2 id id loc  

                            (symf4, i) = paramsC symf3 id params 
                            symf5 = setAddressById symf4 (i,storeEnvId id $literalId d) 
                            
                            (instLoc, symf6, store) = allocC symf5 id loc 
                            instCmd = cmdC symf6 id store cmd pc
             

                            totinstr = [UncondJump (pt + 1)]  ++ instLoc ++ instCmd ++ [Return 1]

                            pc = p + length instLoc +1
                            pt = pc + length instCmd

                alloc sym t c = error $"________________" ++ show t


literalId :: IDecl -> String
literalId (IArrayType i _ _) = i
literalId (IType i _)        = i 
literalId _ = error "no type"

declAllocC :: [Symbol] -> Env -> (IParameter,Int) -> ([Symbol], StackPointer)
declAllocC sym env (IParams  a b, addr) = declAllocC syma env (b, addra)         
        where  (syma, addra) = declAllocC sym env (a, addr) 

declAllocC sym env (IParam _ _ _ lit, addr) = (setAddressById sym (addr, storeEnvId env $literalId lit), addr - 1)

                
paramsC :: [Symbol] -> Env -> IParameter -> ([Symbol], StackPointer)
paramsC sym env params = declAllocC sym env (params, -1)


cmdC :: [Symbol] -> Env -> Storage -> ICmd -> ProgramPointer -> [Instruction]
cmdC sym env store (ICmds a b)  pc = le ++ ri
        where 
                le = cmdC sym env store a pc
                ri = cmdC sym env (addProgPointer store le) b (pc + length le)

cmdC sym env store ISkip  pc = []
cmdC sym env store (IBecomes exl exr)  pc =  il  ++ ir ++ [Store]
        where 
                il = oprL sym env  pc exl
                ir = oprR sym env  (pc + length il)  exr 


cmdC sym env store (IIf expr cmd1 cmd2)  pc = inst
        where 
                addrIf = pc + length expri + length ifi + 2
                addrElse  = addrIf + length eli

                expri = oprR sym env pc expr
                ifi = cmdC sym env store cmd1 (pc + length expri + 1)
                eli = cmdC sym env store cmd2 addrIf
                
                inst = expri ++ [CondJump addrIf] ++ ifi ++ [UncondJump addrElse] ++ eli 

cmdC sym env store (IWhile expr cmd) pc = inst
        where
                addrStart = pc
                addrEnd   = pc + length expri + length loop + 2

                expri   = oprR sym env pc expr 
                loop    = cmdC sym env store cmd  (pc + length expri + 1) 

                inst = expri ++ [CondJump addrEnd] ++ loop ++ [UncondJump addrStart]


cmdC sym env store (IDebugIn expr)  pc = oprL sym env pc expr  ++ [Input (IntTy WL1024) (Loc Nothing) ""]
cmdC sym env store (IDebugOut expr)  pc = oprR sym env pc expr  ++ [Output (IntTy WL1024) ""]


cmdC sym env store b pc =  error $"cmdC " ++show b


addrerssByID sym id = case getSymbol sym id of 
        Just a -> getAddress a
        Nothing -> error $"\n\nno address found " ++ id ++ " in sym table\n\n"++ show sym




oprL :: [Symbol] -> Env -> ProgramPointer  -> IExpr -> [Instruction]
oprL sym env store (ILiteral id _) = 
        case findStoreSymbol sym env id of
                Just (_,_,IStore {},_,addr) -> 
                        if addr >= 0 
                                then [LoadIm IntVmTy (IntVmVal addr)]
                                else [LoadAddrRel addr]


oprL sym env pt (ILiteralArray id expr)  = case findStoreSymbol sym env id of
        Just n -> let 
                addr = getAddress n
                addrLen = addr - 1

                index = oprR sym env pt  expr 

                overflow = index 
                        ++ [LoadIm IntVmTy (IntVmVal addrLen)
                           ,Deref
                           ,Ge Int1024VmTy
                           ,CondJump $pt + length index * 2 + 6]
                        ++ index 
                        ++ [Output (IntTy WL1024) "[Runtime Exception] array out of bounds"
                           ,Stop]
                        


            in  
                        overflow 
                        ++ [LoadIm Int1024VmTy (Int1024VmVal $Int1024 $toInteger addr)] 
                        ++ index
                        ++ [Add Int1024VmTy (Loc Nothing)]
                        ++ [Convert Int1024VmTy IntVmTy (Loc Nothing)]
                -- ++ [LoadIm IntVmTy  (IntVmVal 0)] 

        Nothing  -> error "array L error"  
 




oprL sym _ store n  =  error $"no l value" ++ show n


oprR :: [Symbol] -> Env -> ProgramPointer -> IExpr -> [Instruction]

oprR sym env pc (IExprList id expr) = 
        case findRoutineSymbol sym id of
                Just n -> [AllocBlock 1] ++ oprR sym env pc expr ++ [Call (getAddress n)]
                Nothing -> error $"no function defined " ++ show (routineEnvId "" id) ++ "\n\n" ++ show sym
       
        
oprR sym env pc (IExprListParams a b) = ia ++ ib
        where 
                ia = oprR sym env pc  a 
                ib = oprR sym env (pc + length ia)  b
                
oprR sym env pc (IOpr o e1 e2) = conc $opr' o
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

        opr' t = error $"no oprR" ++ show t

        conc op =  ia ++ ib ++ op
                where 
                        ia = oprR sym env pc  e1
                        ib = oprR sym env (pc + length ia)  e2

oprR sym env pc (IAliteral val) = [LoadIm Int1024VmTy (Int1024VmVal $Int1024 $toInteger val)]
oprR sym env pc (ILiteral id _)   = inc
        
        where inc = case findStoreSymbol sym env id  of 
                Just (_,_,IStore {},_,addr) -> [LoadIm IntVmTy (IntVmVal addr) ,Deref]
                Just (_,_,IType {},_,addr)  -> [LoadAddrRel addr, Deref]
                
                Just x -> error $"no valid label found " ++ show x
                Nothing -> error $"no value found "++ show (storeId id) ++ " " ++ show sym 
oprR sym env pc (IArrayLength id) =  
        case findStoreSymbol sym env id of
                Just n -> let 
                        addrLen = getAddress n - 1
                        in [LoadIm IntVmTy (IntVmVal addrLen) ,Deref]




oprR sym env pt (ILiteralArray id expr) = case findStoreSymbol sym env id of
        Just n -> let 
                addr = getAddress n
                addrLen = addr - 1

                index = oprR sym env pt  expr 

                overflow = index 
                        ++ [LoadIm IntVmTy (IntVmVal addrLen)
                           ,Deref
                           ,Ge Int1024VmTy
                           ,CondJump $pt + length index * 2 + 6]
                        ++ index 
                        ++ [Output (IntTy WL1024) "[Runtime Exception] array out of bounds"
                           ,Stop]
                        


            in  
                        overflow 
                        ++ [LoadIm Int1024VmTy (Int1024VmVal $Int1024 $toInteger addr)] 
                        ++ index
                        ++ [Add Int1024VmTy (Loc Nothing)]
                        ++ [Convert Int1024VmTy IntVmTy (Loc Nothing)]
                        ++ [Deref ]
                -- ++ [LoadIm IntVmTy  (IntVmVal 0)] 

        Nothing  -> error "array L error"  
oprR sym env  store  n  = error $"no supported opr " ++ show n






printSymTable =do
        res <- testProg
        let c = concat $map (\(a,b,c,d,e) -> show (a,b,d,e) ++ ['\n']) (snd res)
        writeFile "./code.sym.vmc" c

printCode =do
        res <- testProg
        let c = concat $map (\n -> show n ++ ['\n']) (fst res)
        writeFile "./code.vmc" c

runProg = do
        (e,l) <- testProg

        let n = listArray (0, length e - 1) e

        let prog = VMProgram (Ident "asdf",  n)
        debugProgram prog


testProg = do
    --file <- readFile "../test/programs/array_sample.iml"
    file <- readFile "../test/programs/p1.iml"

    let val = parseProgram  (scanner file)
    return (genCode $snd (checkProgram val))
    


