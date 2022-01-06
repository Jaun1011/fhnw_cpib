IProg "Euclid" INoParameter (IDeclItem (IDeclItem (IDeclItem (IDeclItem (IDeclItem (IDeclItem (IFunc "certificate" (IParams (IParams (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "a" (VariableType INT32))) (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "b" (VariableType INT32)))) (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "x" (VariableType INT32)))) (IStore (ChangeMode VAR) (IType "c" (VariableType BOOLEAN))) INoParameter INoDecl (IBecomes 
	(ILiteral "c" True)
 
	(IOpr
		(IOpr
			(ILiteral "a" False)
			(ILiteral "b" False)
			AritmeticOperator MULTI)
		(ILiteral "x" False)
		RelOperator EQUAL)
)) (IFunc "less" (IParams (IParams 
        (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "a" (VariableType INT32))) 
        (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "b" (VariableType INT32)))) 
        (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "c" (VariableType INT32)))) 
        
        (IStore (ChangeMode VAR) (IType "x" (VariableType BOOLEAN))) 
        INoParameter 
        INoDecl 
        
        (IBecomes (ILiteral "x" True)
 
	(IOpr
		(IOpr
			(ILiteral "a" False)
			(ILiteral "b" False)
			AritmeticOperator PLUS)
		(ILiteral "c" False)
		RelOperator LESS)
))) (IProc "euclidDivNat" (IParams (IParams (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "a" (VariableType INT32))) (IParam (FlowMode IN) (MechMode COPY) (ChangeMode VAR) (IType "b" (VariableType INT32)))) (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "c" (VariableType INT32)))) INoParameter INoDecl (ICmds (ICmds (IBecomes 
	(ILiteral "g" True)
 
	(ILiteral "a" False)
) (IIf 
	(IOpr
		(ILiteral "g" False)
		(IAliteral 10)
		RelOperator GREATER)
 (IBecomes 
	(ILiteral "g" False)
 
	(ILiteral "t" False)
) (IBecomes 
	(ILiteral "g" False)
 
	(IOpr
		(IAliteral 2)
		(IAliteral 2)
		AritmeticOperator MULTI)
))) (IWhile 
	(IOpr
		(ILiteral "g" False)
		(IAliteral 2)
		RelOperator GREATER)
 (IBecomes 
	(ILiteral "g" False)
 
	(IOpr
		(ILiteral "g" False)
		(IAliteral 1)
		AritmeticOperator MINUS)
))))) (IStore (ChangeMode VAR) (IArrayType "globval" 
	(IAliteral 10)
 (VariableType INT1024)))) (IStore (ChangeMode VAR) (IType "t" (VariableType INT32)))) (IStore (ChangeMode VAR) (IType "g" (VariableType INT32)))) (IStore (ChangeMode VAR) (IType "n" (VariableType BOOLEAN)))) (ICmds (ICmds (IDebugIn 
	(ILiteral "t" True)
) (IBecomes 
	(ILiteral "n" True)
 
	(IExprList "less"
        (IExprListParams
            (IExprListParams
                (ILiteral "t" False)
                (IOpr
                    (IOpr
                        (IAliteral 10)
                        (IAliteral 5)
                        AritmeticOperator PLUS)
                    (IAliteral 1)
                    AritmeticOperator PLUS))
            (IOpr
                (IOpr
                    (IOpr
                        (IAliteral 30)
                        (IAliteral 4)
                        AritmeticOperator MINUS)
                    (IAliteral 5)
                    AritmeticOperator MINUS)
                (IAliteral 6)
                AritmeticOperator MINUS))
)
)) (ICaller "euclidDivNat" 
	(IExprListParams
		(IExprListParams
			(ILiteral "t" False)
			(IAliteral 10))
		(ILiteralArray "globval" 
	(ILiteral "t" False)
))
))