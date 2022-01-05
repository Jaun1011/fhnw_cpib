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


import Symbol (createSymbols, Symbol, getSymbol, getAddress, setAddressById, storeId, getStoreSymbol)
import TypeChecker
import Scanner (scanner)
import Parser (IDecl (IProg, IDeclItem, IStore, IType, IFunc, IProc), parseProgram, IExpr (IOpr, IAliteral, ILiteral), ICmd (ISkip, IBecomes, ICmds, IDebugOut, IDebugIn, IIf, IWhile))
import Model (Attirbute(AritmeticOperator, RelOperator), AritmeticOperator (PLUS, MINUS, MULTI, DIV), Terminal (RELOPR), RelOperator (EQUAL, GREATER, LESS, LESS_EQUAL, GREATER_EQUAL, NOT_EQUAL))
import Debug.Trace
import qualified Vm.BaseDecls as Model


type StackPointer   = Int1024
type ExtremePointer = Int1024
type HeapPointer    = Int1024
type FramePointer   = Int1024

type Storage = (StackPointer, ExtremePointer, HeapPointer, FramePointer)



genCode :: IDecl -> ([Instruction], [Symbol])
genCode (IProg id _ glob cmd) = (allins ++ cmdins ++ [Stop], symalc)
    where 
            sym = createSymbols glob id
            (allins, symalc) = alloc sym 0
            cmdins = cmdC symalc cmd (length allins)
            
            
genCode (IDeclItem a b) = ([],[])
genCode (IStore  _ _) =  ([],[])
genCode _ =  ([],[])



alloc ::  [Symbol]  -> Int -> ([Instruction], [Symbol])
alloc smbl sp = blockAlloc $alloc' smbl (map (\(_,_,n,_,_) -> n) smbl) sp
    where
        blockAlloc :: ([Instruction], [Symbol], Int) -> ([Instruction], [Symbol]) 
        blockAlloc (ins,sym, ep) = (AllocBlock ep : ins, sym)

        alloc' :: [Symbol] -> [IDecl]  -> Int -> ([Instruction], [Symbol], Int)
        alloc' sym (d:ds) sp = (itp ++ it, st, ep2)
                where
                        (itp, st1, ep) = allocDecl d sp
                        (it, st, ep2) = alloc' st1 ds ep


                        allocDecl :: IDecl -> Int -> ([Instruction], [Symbol], Int) 
                        allocDecl (IStore ref (IType id dt)) addr = ([], setAddressById sym (addr, storeId id), addr + 1)
                        allocDecl a b = error $ show a ++ show b
                        
                        {-
                        allocDecl (IFunc id params ret glob loc cmd) = ([],[])
                        allocDecl (IProc id params glob loc cmd)     =  ([],[])
                
                        -}
        alloc' sym [] s  = ([], sym, s)



cmdC :: [Symbol] -> ICmd -> Int -> [Instruction]
cmdC sym (ICmds a b) pc = (le ++ ri) 
        where 
                le = cmdC sym a pc
                ri = cmdC sym b (pc + length le)
cmdC sym ISkip pc = []
cmdC sym (IBecomes l r) pc =  oprL sym l ++ oprR sym r ++ [Store]
cmdC sym (IIf expr cmd1 cmd2) pc = inst
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
                loop    = cmdC sym cmd (pc + length expri + 1)

                inst = expri ++ [CondJump addrEnd] ++ loop ++ [UncondJump addrStart]


cmdC sym (IDebugIn expr) pc = oprL sym expr ++ [Input (IntTy WL1024) (Loc Nothing) ""]
cmdC sym (IDebugOut expr) pc = oprR sym expr ++ [Output (IntTy WL1024) ""]


cmdC sym b pc =  error $"cmdC " ++show b


addrerssByID sym id = case getSymbol sym id of 
        Just a -> getAddress a
        Nothing -> error $"no address found " ++ id ++ "in sym table "++ show sym




oprL :: [Symbol] -> IExpr -> [Instruction]
oprL sym (ILiteral id _) = [LoadIm IntVmTy (IntVmVal (addrerssByID sym $storeId id))]
oprL sym _ =  error $"no l value"


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

        conc op = oprR sym e1 ++ oprR sym e2 ++ op

oprR sym (IAliteral val) = [LoadIm Int1024VmTy (Int1024VmVal $Int1024 $toInteger val)]
oprR sym (ILiteral id _) = [LoadIm IntVmTy (IntVmVal (getAddress s)) ,Deref]
        where s= case getStoreSymbol sym id of 
                Just x -> x -- todo check function
                Nothing -> error $"no value found "++ show id ++ " " ++ show sym 
oprR sym n = error $"no supported opr " ++ show n





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




test = do


    let x = 0

    let alloc = [AllocBlock 1]

    let add addr  =
            [LoadIm IntVmTy     (IntVmVal addr) -- address         
            -- ,LoadIm Int1024VmTy (Int1024VmVal $Int1024 111) --val a


            ,LoadIm IntVmTy (IntVmVal addr)  -- address
            ,Deref

            ,LoadIm Int1024VmTy (Int1024VmVal $Int1024 1) --val b
            ,Add    Int1024VmTy (Loc Nothing)
            ]


    let literal addr =
            [LoadIm IntVmTy (IntVmVal addr)  -- address
            ,Deref]

    let input addr =
            [LoadIm IntVmTy (IntVmVal addr)  -- address
            ,Input (IntTy WL1024) (Loc Nothing) "var" -- input 
            ]

    let output addr =
            [LoadIm IntVmTy (IntVmVal addr)  -- address
            ,Deref
            ,Output (IntTy WL1024) "var"]



    let optest =
            [


            LoadIm Int1024VmTy (Int1024VmVal $Int1024 1) --val b
            ,LoadIm Int1024VmTy (Int1024VmVal $Int1024 1) --val b
            ,Add    Int1024VmTy (Loc Nothing)


            ,Output (IntTy WL1024) "var"]

    let e = alloc
                ++ optest
            ++ [


               -- (sp = 4) 0

            Stop

            {-
            AllocBlock 1                                   -- (sp = 0)

            ,LoadIm IntVmTy (IntVmVal 0)                    -- (sp = 1)
            ,Input (IntTy WL32) (Loc Nothing) "x" --input -- (sp = 2)
            
            ,LoadIm IntVmTy (IntVmVal 0)                    -- (sp = 3) 0
            ,LoadIm IntVmTy (IntVmVal 0)                    -- (sp = 4) 0
            ,Deref                                          -- (sp = 4) 
            ,LoadIm IntVmTy (IntVmVal 22)                    -- (sp = 4) 0
            ,LoadIm IntVmTy (IntVmVal 22)                    -- (sp = 4) 0
            ,Add IntVmTy (Loc Nothing)
   

            ,Stop


            
            ,LoadIm Int32VmTy (Int32VmVal (Int32 0))
            ,LoadIm Int32VmTy (Int32VmVal (Int32 0))
            ,Deref
            ,LoadIm Int32VmTy (Int32VmVal (Int32 17))
            ,Add Int32VmTy (Loc Nothing)
            ,Store
            ,LoadIm Int32VmTy (Int32VmVal (Int32 0))
            ,Deref
             
            -}
            ]

    let n = listArray (0, length e - 1) e

    let prog = VMProgram (Ident "asdf",  n)
    debugProgram prog