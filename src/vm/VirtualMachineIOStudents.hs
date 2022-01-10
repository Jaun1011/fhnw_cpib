-- BasicIML V01
-- Edgar F.A. Lederer, FHNW and Uni Basel, 2015
-- January 2020
-- December 2020
-- December 2021

module Vm.VirtualMachineIOStudents
  (VmType(..), VmValue(..), Instruction(..),
   CodeAddress, StoreAddress, Code, VMProgram(VMProgram),
   execProgram, debugProgram)
where

import Vm.BaseDecls
import Vm.CheckedArithmetic
import Vm.ErrorHandlingGeneric
import Vm.Locations
--import Scanner(readBool, readInteger)
import Vm.VmData

import Data.Array
import qualified Data.Char(digitToInt, isDigit)

-- version for students without using Scanner
readBool :: String -> Maybe Bool
readBool s =
  case trim s of
    "false" -> Just False
    "ff"    -> Just False
    "true"  -> Just True
    "tt"    -> Just True
    _ -> Nothing

readInteger :: String -> Maybe Integer
readInteger s = f (trim s)
  where
    f ('+' : cs) = g cs
    f ('-' : cs) = fmap negate (g cs)
    f cs = g cs
    g (c : cs)
      | Data.Char.isDigit c = h cs (digitToInteger c)
      | otherwise = Nothing
    g [] = Nothing
    h (c : cs) accu
      | Data.Char.isDigit c = h cs (10 * accu + digitToInteger c)
      | otherwise = Nothing
    h [] accu = Just accu

trim :: String -> String
trim = reverse . dropSpaces . reverse . dropSpaces
  where dropSpaces = dropWhile (== ' ')

digitToInteger :: Char -> Integer
digitToInteger = toInteger . Data.Char.digitToInt

type PC = CodeAddress -- program counter
type FP = StoreAddress -- frame pointer
type Stack = [VmValue]
type State = (PC, FP, Stack)

unspecVmVal :: VmValue
unspecVmVal = IntVmVal 0

readS :: Stack -> StoreAddress -> VmValue
readS stack addr = stack !! ((length stack - 1) - addr)

updateS :: Stack -> (StoreAddress, VmValue) -> Stack
updateS stack (addr, val) = stack'
  where
    stack' = top ++ (val : bottom)
    (top, _ : bottom) = splitAt ((length stack - 1) - addr) stack

return2 :: State -> IO (Checked State)
return2 = return . return

execInstr :: Instruction -> State -> IO (Checked State)
execInstr Dup (pc, fp, val : stack) =
  return2 (pc + 1, fp, val : val : stack)
execInstr (AllocBlock size) (pc, fp, stack) =
  return2 (pc + 1, fp, replicate size unspecVmVal ++ stack)
execInstr (AllocStack _maxSize) (pc, fp, stack) =
  return2 (pc + 1, fp, stack)
execInstr (Call routAddr) (pc, fp, stack) =
    return2 (routAddr, fp', stack')
  where
    fp' = length stack
    stack' = IntVmVal pc : unspecVmVal : IntVmVal fp : stack
execInstr (Return size) (_, fp, stack) =
    return2 (pc + 1, fp', stack'')
  where
    IntVmVal pc : _ : IntVmVal fp' : stack' =
      drop (length stack - (fp + 3)) stack
    stack'' = drop size stack'

execInstr (LoadIm IntVmTy val@(IntVmVal _)) (pc, fp, stack) =
  return2 (pc + 1, fp, val : stack)
execInstr (LoadIm Int32VmTy val@(Int32VmVal _)) (pc, fp, stack) =
  return2 (pc + 1, fp, val : stack)
execInstr (LoadIm Int64VmTy val@(Int64VmVal _)) (pc, fp, stack) =
  return2 (pc + 1, fp, val : stack)
execInstr (LoadIm Int1024VmTy val@(Int1024VmVal _)) (pc, fp, stack) =
  return2 (pc + 1, fp, val : stack)

execInstr (LoadAddrRel relAddr) (pc, fp, stack) =
  return2 (pc + 1, fp, IntVmVal (fp + relAddr) : stack)
execInstr Deref (pc, fp, IntVmVal addr : stack) =
  return2 (pc + 1, fp, readS stack addr : stack)
execInstr Store (pc, fp, val : IntVmVal addr : stack) =
  return2 (pc + 1, fp, updateS stack (addr, val))

execInstr (Neg Int32VmTy loc) (pc, fp, Int32VmVal x : stack) =
  case negExcla x of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "negate: overflow of 32 bit")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (Neg Int64VmTy loc) (pc, fp, Int64VmVal x : stack) =
  case negExcla x of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "negate: overflow of 64 bit")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (Neg Int1024VmTy loc) (pc, fp, Int1024VmVal x : stack) =
  case negExcla x of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "negate: overflow of 1024 bit")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (Add Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x +! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "add: overflow of 32 bit")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (Add Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x +! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "add: overflow of 64 bit")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (Add Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x +! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "add: overflow of 1024 bit")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (Sub Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x -! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "sub: overflow of 32 bit")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (Sub Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x -! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "sub: overflow of 64 bit")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (Sub Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x -! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "sub: overflow of 1024 bit")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (Mult Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x *! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "mult: overflow of 32 bit")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (Mult Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x *! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "mult: overflow of 64 bit")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (Mult Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x *! y of
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "mult: overflow of 1024 bit")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (DivEuclid Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x `divEexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divE: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divE: overflow of 32 bit")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (DivEuclid Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x `divEexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divE: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divE: overflow of 64 bit")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (DivEuclid Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x `divEexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divE: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divE: overflow of 1024 bit")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (ModEuclid Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x `modEexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modE: division by zero")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (ModEuclid Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x `modEexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modE: division by zero")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (ModEuclid Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x `modEexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modE: division by zero")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (DivFloor Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x `divFexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divF: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divF: overflow of 32 bit")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (DivFloor Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x `divFexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divF: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divF: overflow of 64 bit")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (DivFloor Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x `divFexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divF: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divF: overflow of 1024 bit")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (ModFloor Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x `modFexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modF: division by zero")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (ModFloor Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x `modFexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modF: division by zero")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (ModFloor Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x `modFexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modF: division by zero")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (DivTrunc Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x `divTexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divT: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divT: overflow of 32 bit")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (DivTrunc Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x `divTexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divT: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divT: overflow of 64 bit")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (DivTrunc Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x `divTexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "divT: division by zero")))
    Left Overflow ->
      return (Left (ErrorMsg ([loc], "divT: overflow of 1024 bit")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (ModTrunc Int32VmTy loc) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  case x `modTexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modT: division by zero")))
    Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
execInstr (ModTrunc Int64VmTy loc) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  case x `modTexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modT: division by zero")))
    Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)
execInstr (ModTrunc Int1024VmTy loc) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  case x `modTexcla` y of
    Left DivisionByZero ->
      return (Left (ErrorMsg ([loc], "modT: division by zero")))
    Right result -> return2 (pc + 1, fp, Int1024VmVal result : stack)

execInstr (Eq IntVmTy) (pc, fp, IntVmVal y : IntVmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x == y)) : stack)
execInstr (Eq Int32VmTy) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x == y)) : stack)
execInstr (Eq Int64VmTy) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x == y)) : stack)
execInstr (Eq Int1024VmTy) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x == y)) : stack)

execInstr (Ne IntVmTy) (pc, fp, IntVmVal y : IntVmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x /= y)) : stack)
execInstr (Ne Int32VmTy) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x /= y)) : stack)
execInstr (Ne Int64VmTy) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x /= y)) : stack)
execInstr (Ne Int1024VmTy) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x /= y)) : stack)

execInstr (Lt Int32VmTy) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x < y)) : stack)
execInstr (Lt Int64VmTy) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x < y)) : stack)
execInstr (Lt Int1024VmTy) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x < y)) : stack)

execInstr (Le Int32VmTy) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x <= y)) : stack)
execInstr (Le Int64VmTy) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x <= y)) : stack)
execInstr (Le Int1024VmTy) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x <= y)) : stack)

execInstr (Gt Int32VmTy) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x > y)) : stack)
execInstr (Gt Int64VmTy) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x > y)) : stack)
execInstr (Gt Int1024VmTy) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x > y)) : stack)

execInstr (Ge Int32VmTy) (pc, fp, Int32VmVal y : Int32VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x >= y)) : stack)
execInstr (Ge Int64VmTy) (pc, fp, Int64VmVal y : Int64VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x >= y)) : stack)
execInstr (Ge Int1024VmTy) (pc, fp, Int1024VmVal y : Int1024VmVal x : stack) =
  return2 (pc + 1, fp, IntVmVal (boolToInt (x >= y)) : stack)

execInstr (Convert fromTy toTy loc) (pc, fp, vmVal : stack) =
  case (fromTy, vmVal, toTy) of
    (Int32VmTy, Int32VmVal a, Int64VmTy)    -> return2 (pc + 1, fp, Int64VmVal (fromInt32toInt64 a) : stack)
    (Int32VmTy, Int32VmVal a, Int1024VmTy)  -> return2 (pc + 1, fp, Int1024VmVal (fromInt32toInt1024 a) : stack)
    (Int64VmTy, Int64VmVal a, Int1024VmTy)  -> return2 (pc + 1, fp, Int1024VmVal (fromInt64toInt1024 a) : stack)
    (Int64VmTy, Int64VmVal a, Int32VmTy)    -> 
        case fromInt64toInt32 a of
            Left Overflow -> return (Left (ErrorMsg ([loc], "convert: overflow from 64 to 32 bit")))
            Right result  -> return2 (pc + 1, fp, Int32VmVal result : stack)
    (Int1024VmTy, Int1024VmVal a, Int32VmTy) ->
      case fromInt1024toInt32 a of
        Left Overflow -> return (Left (ErrorMsg ([loc], "convert: overflow from 1024 to 32 bit")))
        Right result -> return2 (pc + 1, fp, Int32VmVal result : stack)
    (Int1024VmTy, Int1024VmVal a, Int64VmTy) ->
      case fromInt1024toInt64 a of
        Left Overflow -> return (Left (ErrorMsg ([loc], "convert: overflow from 1024 to 64 bit")))
        Right result -> return2 (pc + 1, fp, Int64VmVal result : stack)

execInstr Stop (_, fp, stack) =
  return2 (-1, fp, stack)
execInstr (UncondJump jumpAddr) (_, fp, stack) =
  return2 (jumpAddr, fp, stack)
execInstr (CondJump jumpAddr) (pc, fp, IntVmVal x : stack)
  | intToBool x = return2 (pc + 1,   fp, stack) -- true
  | otherwise   = return2 (jumpAddr, fp, stack) -- false

execInstr (Input BoolTy loc indicator) (pc, fp, IntVmVal addr : stack) =
  do putStr ("? " ++ indicator ++ " : " ++ show BoolTy ++ " = ");
     inputString <- getLine
     case readBool inputString of
       Nothing ->
         return (Left (ErrorMsg ([loc], "input: not a boolean literal")))
       Just b ->
         let inputVmVal = (IntVmVal . boolToInt) b
             stack' = updateS stack (addr, inputVmVal)
         in return2 (pc + 1, fp, stack')
execInstr (Input (IntTy WL32) loc indicator) (pc, fp, IntVmVal addr : stack) =
  do putStr ("? " ++ indicator ++ " : " ++ show (IntTy WL32) ++ " = ");
     inputString <- getLine
     case readInteger inputString of
       Nothing ->
         return (Left (ErrorMsg ([loc], "input: not an integer literal")))
       Just i ->
         case fromIntegerToInt32 i of
           Left Overflow ->
             return (Left (ErrorMsg ([loc], "input: overflow of 32 bit")))
           Right result ->
             let stack' = updateS stack (addr, Int32VmVal result)
             in return2 (pc + 1, fp, stack')
execInstr (Input (IntTy WL64) loc indicator) (pc, fp, IntVmVal addr : stack) =
  do putStr ("? " ++ indicator ++ " : " ++ show (IntTy WL64) ++ " = ");
     inputString <- getLine
     case readInteger inputString of
       Nothing ->
         return (Left (ErrorMsg ([loc], "input: not an integer literal")))
       Just i ->
         case fromIntegerToInt64 i of
           Left Overflow ->
             return (Left (ErrorMsg ([loc], "input: overflow of 64 bit")))
           Right result ->
             let stack' = updateS stack (addr, Int64VmVal result)
             in return2 (pc + 1, fp, stack')
execInstr (Input (IntTy WL1024) loc indicator) (pc, fp, IntVmVal addr : stack) =
  do putStr ("? " ++ indicator ++ " : " ++ show (IntTy WL1024) ++ " = ");
     inputString <- getLine
     case readInteger inputString of
       Nothing ->
         return (Left (ErrorMsg ([loc], "input: not an integer literal")))
       Just i ->
         case fromIntegerToInt1024 i of
           Left Overflow ->
             return (Left (ErrorMsg ([loc], "input: overflow of 1024 bit")))
           Right result ->
             let stack' = updateS stack (addr, Int1024VmVal result)
             in return2 (pc + 1, fp, stack')

execInstr (Output BoolTy indicator) (pc, fp, IntVmVal val : stack) =
  do putStrLn ("! " ++ indicator ++ " = " ++ show (intToBool val) ++ " : " ++ show BoolTy)
     return2 (pc + 1, fp, stack)
execInstr (Output (IntTy WL32) indicator) (pc, fp, Int32VmVal val : stack) =
  do putStrLn ("! " ++ indicator ++ " = " ++ show val ++ " : " ++ show (IntTy WL32))
     return2 (pc + 1, fp, stack)
execInstr (Output (IntTy WL64) indicator) (pc, fp, Int64VmVal val : stack) =
  do putStrLn ("! " ++ indicator ++ " = " ++ show val ++ " : " ++ show (IntTy WL64))
     return2 (pc + 1, fp, stack)
execInstr (Output (IntTy WL1024) indicator) (pc, fp, Int1024VmVal val : stack) =
  do putStrLn ("! " ++ indicator ++ " = " ++ show val ++ " : " ++ show (IntTy WL1024))
     return2 (pc + 1, fp, stack)
execInstr instr _ =
  return (Left (ErrorMsg ([], "internal error VM: " ++ show instr)))

execProgram :: VMProgram -> IO (Checked Ident)
execProgram (VMProgram (progId, codeArray)) = run (Right (0, 0, []))
  where
    run (Right state@(pc, _, _))
      | pc >= 0 = do checkState <- execInstr (codeArray ! pc) state
                     run checkState
      | otherwise = return (return progId)
    run (Left errMsg) = return (Left errMsg)

debugProgram :: VMProgram -> IO (Checked Ident, Code)
debugProgram (VMProgram (progId, codeArray)) = run (Right (0, 0, [])) []
  where
    run (Right state@(pc, _, _)) instrAccu
      | pc >= 0 = do let instr = codeArray ! pc
                     checkState <- execInstr instr state
                     run checkState (instr : instrAccu)
      | otherwise = return (return progId, reverse instrAccu)
    run (Left errMsg) instrAccu = return (Left errMsg, reverse instrAccu)
