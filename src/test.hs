[AllocBlock 3
,LoadIm Int 1
,Input int1024 --- ""
,LoadIm Int 2
,LoadIm Int 1
,Deref
,Store
,LoadIm Int 0
,LoadIm Int 2
,Deref
,LoadIm Int 1
,Deref
,Gt Int1024
,Storetest = 
,CondJump 18
,LoadIm Int1024 0
,Output int1024 ""
,UncondJump 21
,LoadIm Int1024 1
,Output int1024 ""
,LoadIm Int 2
,Deref
,Output int1024 ""
,Stop]