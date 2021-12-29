import Scanner (scanner)
import Parser






loadProg = do
    file <- readFile "../test/programs/array_sample.iml"
    return $parseProgram (scanner file)






typeCheck :: IDecl -> [IDecl] 
typeCheck (IProg id params glob loc) = typeCheck glob
typeCheck a = [a]



