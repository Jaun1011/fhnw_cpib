module Compiler (run) where

    
import Parser (parseProgram)
import Scanner (scanner)
import TypeChecker (checkProgram)
import GHC.IO (FilePath)
import Utils.Logger (info)
import CodeGenerator (runProg, genCode)



mainpath = "../test/programs/"
outpath = mainpath ++ "out/"

run :: FilePath -> IO ()
run file = do 
    file <- readFile (mainpath ++ file)
    let ast = parseProgram (scanner file)
    let check = checkProgram ast
    let symFile = outpath ++ file ++ ".sym"
    printCode "typechecker" check


    let (inst, sym) = genCode ast
    printSymTable "codegen" sym
    printCode "codegen" inst



    runProg inst






printSymTable name res =do
        let c = concat $map (\(a,b,c,d,e) -> show (a,b,d,e) ++ ['\n']) res
        writeFile (outpath ++ name ++ ".sym") c

printCode name res = do
        let c = concat $map (\n -> show n ++ ['\n']) (res)
        writeFile (outpath ++ name  ++ ".inst") c
{-


testProg = do
    --file <- readFile "../test/programs/array_sample.iml"
    file <- readFile "../test/programs/p1.iml"

    let val = parseProgram  (scanner file)
    return (genCode (checkProgram val))
    
-}