import Scanner (scanner)
import Parser





type Symbol = (IDecl)



loadProg = do
    file <- readFile "../test/programs/array_sample.iml"
    return $parseProgram (scanner file)


typeCheck :: IDecl -> [Symbol]
