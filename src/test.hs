IProg "Euclid" INoParameter (IDeclItem (IDeclItem (IDeclItem (IDeclItem (IDeclItem (IFunc "certificate" (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "a" (VariableType INT1024))) (IStore (ChangeMode VAR) (IType "c" (VariableType BOOLEAN))) INoParameter INoDecl (IBecomes 
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
)) (IFunc "less" (IParams (IParams (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "a" (VariableType INT1024))) (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "b" (VariableType INT1024)))) (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "c" (VariableType INT1024)))) (IStore (ChangeMode VAR) (IType "x" (VariableType BOOLEAN))) INoParameter INoDecl (IBecomes 
	(ILiteral "c" True)
 
	(IOpr
		(ILiteral "c" False)
		(IOpr
			(ILiteral "a" False)
			(ILiteral "b" False)
			AritmeticOperator PLUS)
		RelOperator LESS)
))) (IProc "euclidDivNat" (IParams (IParams (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "a" (VariableType INT1024))) (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "b" (VariableType INT1024)))) (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "c" (VariableType INT1024)))) INoParameter (IStore (ChangeMode VAR) (IType "g" (VariableType INT1024))) (ICmds (ICmds (ICmds (IBecomes 
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
 (VariableType INT1024)))) (IStore (ChangeMode VAR) (IType "a" (VariableType INT32)))) (IStore (ChangeMode VAR) (IType "n" (VariableType BOOLEAN)))) (ICmds (ICmds (IBecomes 
	(ILiteral "a" True)
 
	(IAliteral 10)
) (IBecomes 
	(ILiteral "n" False)
 
	(IExprList "less"
	(IExprListParams
		(IExprListParams
			(ILiteral "a" False)
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
			(ILiteral "a" False)
			(IAliteral 10))
		(IAliteral 30))
))