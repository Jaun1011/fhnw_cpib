-- BasicIML V01
-- Edgar F.A. Lederer, FHNW
-- December 2020

module Vm.VmData where

import Vm.BaseDecls
import Vm.CheckedArithmetic
import Vm.Locations
import Vm.Utilities(flushRight)

import Data.Array ( Array, assocs, bounds )

type StoreAddress = Int
type CodeAddress = Int
type Code = [Instruction]
type CodeArray = Array CodeAddress Instruction

newtype VMProgram = VMProgram (Ident, CodeArray)

instance Show VMProgram where
  show (VMProgram (Ident progId, codeArray)) =
      progId ++ ";\n" ++ show1 (assocs codeArray)
    where
      show1 = concatMap show2
      show2 (num, instr) =
        flushRight widthForNums (show num) ++ " : " ++ show instr ++ ";\n"
      widthForNums = length (show high)
      (_, high) = bounds codeArray

data VmType
  = IntVmTy
  | Int32VmTy
  | Int64VmTy
  | Int1024VmTy
  deriving Eq

instance Show VmType where
  show IntVmTy     = "Int"
  show Int32VmTy   = "Int32"
  show Int64VmTy   = "Int64"
  show Int1024VmTy = "Int1024"

data VmValue
  = IntVmVal Int
  | Int32VmVal Int32
  | Int64VmVal Int64
  | Int1024VmVal Int1024
  deriving Eq

instance Show VmValue where
  show (IntVmVal v)               = show v
  show (Int32VmVal (Int32 v))     = show v
  show (Int64VmVal (Int64 v))     = show v
  show (Int1024VmVal (Int1024 v)) = show v

data Instruction
  = Stop
  | Dup
  | AllocBlock Int
  | AllocStack Int
  | Call CodeAddress
  | Return Int
  | LoadIm VmType VmValue
  | LoadAddrRel Int
  | Deref
  | Store
  | Neg VmType Location
  | Add VmType Location
  | Sub VmType Location
  | Mult VmType Location
  | DivEuclid VmType Location
  | ModEuclid VmType Location
  | DivFloor VmType Location
  | ModFloor VmType Location
  | DivTrunc VmType Location
  | ModTrunc VmType Location
  | Eq VmType
  | Ne VmType
  | Gt VmType
  | Ge VmType
  | Lt VmType
  | Le VmType
  | Convert VmType VmType Location
  | UncondJump CodeAddress
  | CondJump CodeAddress
  | Input Type Location String
  | Output Type String
  deriving Show

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

intToBool :: Int -> Bool
intToBool 0 = False
intToBool 1 = True
