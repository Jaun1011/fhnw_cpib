
testcmds :: [Bool]
testcmds = map (\(i,o) -> checkCmd sym (parseEx i) == o) testSuite
        where

            testSuite = [("x := less(a1,a1,a1)" , sym)
                        ,("call euklic(a1,a1,a1)" , sym)
                        ,("con init := 12" , sym)
                       --,("con2 := 12" , sym)
                

                        ,("if a1 = a2 then a2 := a3 endif" , sym)
                        ,("if a1 = a2 then a2 := a3 else y := a1 = a2 endif" , sym)

                        ,("while a1 = a2 do a2 := a3  endwhile" , sym)

                        ,("debugin a1 = a2" , sym)
                        ,("debugout a1 = a2" , sym)
                        -- errorcase 
                        --("while a2 do a2 := a3  endwhile" , sym),
                        --("if a1 then a2 := a3 else y := a1 = a2 endif" , sym),


                        ,("a3 := a1 + a2" , sym)
                        ,("y := a1 = a2" , sym)
                        ,("a3 := a1 + arraya[0]  + arrayb[a2 + 1]" , sym)
                        ,("z := x /\\? y" , sym)

                        ,("init1 init := init2" , sym)
                        ,("arraya := arrayb" , sym)
                ]
                --]

            parseEx a = parseCmds (scanner a)



            sym :: [Symbol]
            sym = [
                ("global","store.a1", IStore (ChangeMode  VAR) $IType "a1" (VariableType INT32),True, 1),
                ("global","store.a2", IStore (ChangeMode  VAR) $IType "a2" (VariableType INT32),True, 1),
                ("global","store.a3", IStore (ChangeMode  VAR) $IType "a3" (VariableType INT32),True, 1),

                ("global","store.con", IStore (ChangeMode  CONST) $IType "con" (VariableType INT32),False , 1),
                ("global","store.con2", IStore (ChangeMode  CONST) $IType "con" (VariableType INT32),True, 1),

                ("global","store.init1", IStore (ChangeMode  VAR) $IType "init1" (VariableType INT32),False, 1),
                ("global","store.init2", IStore (ChangeMode  VAR) $IType "init2" (VariableType INT32),True, 1),

                ("global","store.arraya", IStore (ChangeMode  VAR) $IArrayType  "arraya" (IAliteral 10) (VariableType INT32),True, 1),
                ("global","store.arrayb", IStore (ChangeMode  VAR) $IArrayType  "arrayb" (IAliteral 10) (VariableType INT32),True, 1),
                
                ("global","store.x", IStore (ChangeMode  VAR) $IType "x" (VariableType BOOLEAN),True, 1),
                ("global","store.y", IStore (ChangeMode  VAR) $IType "y" (VariableType BOOLEAN ),True, 1),
                ("global","store.z", IStore (ChangeMode  VAR) $IType "y" (VariableType BOOLEAN ),True, 1),
                
                ("global","store.ab", IStore (ChangeMode  VAR) $IType "ab" (VariableType INT64),True, 1),
                ("global","routine.euklic", (IProc "euklic" (IParams 
                                            (IParams (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "a" (VariableType INT32)))
                                                (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "b" (VariableType INT32)))) 
                                                (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "c" (VariableType INT32)))) 
                                            INoParameter (IStore (ChangeMode VAR) 
                                            (IType "g" (VariableType INT32))) 
                                            ISkip)
                , True, 1),
                ("global","routine.less", (IFunc "less" (IParams 
                                            (IParams 
                                                (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "a" (VariableType INT32))) 
                                                (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "b" (VariableType INT32)))) 
                                            (IParam (FlowMode IN) (MechMode REF) (ChangeMode VAR) (IType "c" (VariableType INT32)))) 
                                        (IStore (ChangeMode VAR) (IType "x" (VariableType BOOLEAN))) 
                                        INoParameter 
                                        INoDecl 
                                        ISkip), True, 1)]


testSym = trace "sym" (setAddressById sym (111111111111, "routine.euklic")) 
    where
        sym = [
                ("global","store.a1", IStore (ChangeMode  VAR) $IType "a1" (VariableType INT32),True, 1),
                ("global","store.a2", IStore (ChangeMode  VAR) $IType "a2" (VariableType INT32),True, 1),
                ("global","store.a3", IStore (ChangeMode  VAR) $IType "a3" (VariableType INT32),True, 1),

                ("global","store.con", IStore (ChangeMode  CONST) $IType "con" (VariableType INT32),False , 1),
                ("global","store.con2", IStore (ChangeMode  CONST) $IType "con" (VariableType INT32),True, 1),

                ("global","store.init1", IStore (ChangeMode  VAR) $IType "init1" (VariableType INT32),False, 1),
                ("global","store.init2", IStore (ChangeMode  VAR) $IType "init2" (VariableType INT32),True, 1),

                ("global","store.arraya", IStore (ChangeMode  VAR) $IArrayType  "arraya" (IAliteral 10) (VariableType INT32),True, 1),
                ("global","store.arrayb", IStore (ChangeMode  VAR) $IArrayType  "arrayb" (IAliteral 10) (VariableType INT32),True, 1),
                
                ("global","store.x", IStore (ChangeMode  VAR) $IType "x" (VariableType BOOLEAN),True, 1),
                ("global","store.y", IStore (ChangeMode  VAR) $IType "y" (VariableType BOOLEAN ),True, 1),
                ("global","store.z", IStore (ChangeMode  VAR) $IType "y" (VariableType BOOLEAN ),True, 1),
                
                ("global","store.ab", IStore (ChangeMode  VAR) $IType "ab" (VariableType INT64),True, 1),
                ("global","routine.euklic", (IProc "euklic" (IParams 
                                            (IParams (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "a" (VariableType INT32)))
                                                (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "b" (VariableType INT32)))) 
                                                (IParam (FlowMode IN) (MechMode COPY) (ChangeMode CONST) (IType "c" (VariableType INT32)))) 
                                            INoParameter (IStore (ChangeMode VAR) 
                                            (IType "g" (VariableType INT32))) 
                                            ISkip)
                , True, 1)                        
                ]