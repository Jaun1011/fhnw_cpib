IProg "Euclid" INoParameter (IDeclItem (IDeclItem (IDeclItem (IDeclItem (IFunc "certificate" (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "a" (VariableType INT1024))) (IStore (ChangeMode VAR) (IType "c" (VariableType BOOLEAN))) INoParameter INoDecl (IBecomes 
	(ILiteral "c" True)
 
	(IOpr
		(IOpr
			(ILiteral "a" False)
			(IOpr
				(IOpr
					(IMonadic
						(ILiteral "sign" False)
						(INone)
						AritmeticOperator PLUS)
					(ILiteral "v'" False)
					AritmeticOperator MULTI)
				(ILiteral "g" False)
				AritmeticOperator MULTI)
			RelOperator EQUAL)
		(IOpr
			(ILiteral "b" False)
			(IOpr
				(IOpr
					(IMonadic
						(ILiteral "sign" False)
						(INone)
						AritmeticOperator MINUS)
					(ILiteral "u'" False)
					AritmeticOperator MULTI)
				(ILiteral "g" False)
				AritmeticOperator MULTI)
			RelOperator EQUAL)
		LogicOperator AND)
)) (IFunc "certificate22" (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "a" (VariableType INT1024))) (IStore (ChangeMode VAR) (IType "c" (VariableType BOOLEAN))) INoParameter INoDecl (IBecomes 
	(ILiteral "c" True)
 
	(IOpr
		(IOpr
			(ILiteral "a" False)
			(IOpr
				(IOpr
					(IMonadic
						(ILiteral "sign" False)
						(INone)
						AritmeticOperator PLUS)
					(ILiteral "v'" False)
					AritmeticOperator MULTI)
				(ILiteral "g" False)
				AritmeticOperator MULTI)
			RelOperator EQUAL)
		(IOpr
			(ILiteral "b" False)
			(IOpr
				(IOpr
					(IMonadic
						(ILiteral "sign" False)
						(INone)
						AritmeticOperator MINUS)
					(ILiteral "u'" False)
					AritmeticOperator MULTI)
				(ILiteral "g" False)
				AritmeticOperator MULTI)
			RelOperator EQUAL)
		LogicOperator AND)
))) (IProc "euclidDivNat" (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "a" (VariableType INT1024))) INoParameter (IStore (ChangeMode VAR) (IType "g" (VariableType INT1024))) (ICmds (ICmds (ICmds (IBecomes 
	(ILiteral "g" True)
 
	(ILiteral "a" False)
) (IBecomes 
	(ILiteral "numIt" True)
 
	(IAliteral 0)
)) (IIf 
	(IOpr
		(IAliteral 0)
		(ILiteral "g" False)
		RelOperator EQUAL)
 (IBecomes 
	(ILiteral "g" False)
 
	(IAliteral 1)
) (IBecomes 
	(ILiteral "g" False)
 
	(IOpr
		(IAliteral 2)
		(IAliteral 2)
		AritmeticOperator MULTI)
))) (IWhile 
	(IOpr
		(IAliteral 2)
		(ILiteral "g" False)
		RelOperator GREATER)
 (IBecomes 
	(ILiteral "g" False)
 
	(IOpr
		(ILiteral "g" False)
		(IAliteral 1)
		AritmeticOperator MINUS)
))))) (IStore (ChangeMode VAR) (IArrayType "globval" 
	(IAliteral 10)
 (VariableType INT1024)))) (IStore (ChangeMode VAR) (IType "a" (VariableType INT32)))) (ICmds (IBecomes 
	(ILiteral "a" True)
 
	(IAliteral 10)
) (ICaller "test" 
	(IExprListParams
		(IExprListParams
			(ILiteral "globval" False)
			(ILiteralArray "globval" 
	(IOpr
		(IAliteral 1)
		(ILiteral "a" False)
		AritmeticOperator PLUS)
))
		(IOpr
			(IAliteral 10)
			(ILiteral "a" False)
			AritmeticOperator PLUS))
))