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
    info "typechecker symtable" check


    let (inst, sym) = genCode ast
    runProg inst

