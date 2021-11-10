
(*
generated by jku, ica
time = 2021-11-10 23:55:25
*)
datatype nonterm
    = program
    | progParamList
    | cpsDecl
    | cpsCmd
    | decl
    | stoDecl
    | funDecl
    | procDecl
    | typedIdent
    | paramList
    | globImps
    | cpsStoDecl
    | globImp
    | progParam
    | param
    | atomtype
    | array
    | cmd
    | expr
    | exprList
    | globInits
    | idents
    | ident
    | arraylen
    | term1
    | term2
    | term3
    | factor
    | monadicOpr
    | opt0
    | opt1
    | opt2
    | opt3
    | opt4
    | opt5
    | rep0
    | opt6
    | opt7
    | rep1
    | rep2
    | opt8
    | opt9
    | opt10
    | opt11
    | opt12
    | opt13
    | opt14
    | joice3
    | opt15
    | rep3
    | rep4
    | rep5
    | joice6
    | opt16
    | rep6
    | opt17
    | rep7
    | rep8
    | opt18
    | opt19
    | rep9
    | rep10
    | joice11
    | rep11

val string_of_nonterm =
    fn program => "program"
    | progParamList => "progParamList"
    | cpsDecl => "cpsDecl"
    | cpsCmd => "cpsCmd"
    | decl => "decl"
    | stoDecl => "stoDecl"
    | funDecl => "funDecl"
    | procDecl => "procDecl"
    | typedIdent => "typedIdent"
    | paramList => "paramList"
    | globImps => "globImps"
    | cpsStoDecl => "cpsStoDecl"
    | globImp => "globImp"
    | progParam => "progParam"
    | param => "param"
    | atomtype => "atomtype"
    | array => "array"
    | cmd => "cmd"
    | expr => "expr"
    | exprList => "exprList"
    | globInits => "globInits"
    | idents => "idents"
    | ident => "ident"
    | arraylen => "arraylen"
    | term1 => "term1"
    | term2 => "term2"
    | term3 => "term3"
    | factor => "factor"
    | monadicOpr => "monadicOpr"
    | opt0 => "opt0"
    | opt1 => "opt1"
    | opt2 => "opt2"
    | opt3 => "opt3"
    | opt4 => "opt4"
    | opt5 => "opt5"
    | rep0 => "rep0"
    | opt6 => "opt6"
    | opt7 => "opt7"
    | rep1 => "rep1"
    | rep2 => "rep2"
    | opt8 => "opt8"
    | opt9 => "opt9"
    | opt10 => "opt10"
    | opt11 => "opt11"
    | opt12 => "opt12"
    | opt13 => "opt13"
    | opt14 => "opt14"
    | joice3 => "joice3"
    | opt15 => "opt15"
    | rep3 => "rep3"
    | rep4 => "rep4"
    | rep5 => "rep5"
    | joice6 => "joice6"
    | opt16 => "opt16"
    | rep6 => "rep6"
    | opt17 => "opt17"
    | rep7 => "rep7"
    | rep8 => "rep8"
    | opt18 => "opt18"
    | opt19 => "opt19"
    | rep9 => "rep9"
    | rep10 => "rep10"
    | joice11 => "joice11"
    | rep11 => "rep11"

datatype term
    = PROGRAM
    | IDENT
    | GLOBAL
    | DO
    | ENDPROGRAM
    | CHANGEMODE
    | FUN
    | RETURNS
    | LOCAL
    | ENDFUN
    | PROC
    | ENDPROC
    | COMMA
    | FLOWMODE
    | SEMICOLON
    | LPAREN
    | RPAREN
    | MECHMODE
    | COLON
    | INT
    | INT32
    | INT1024
    | BOOL
    | SKIP
    | BECOMES
    | IF
    | THEN
    | ELSE
    | ENDIF
    | WHILE
    | ENDWHILE
    | CALL
    | DEBUGIN
    | DEBUGOUT
    | INIT
    | CHAR
    | NUMBER
    | LEBRKT
    | REBRKT
    | DOT
    | LENGTH
    | BOOLOPR
    | RELOPR
    | ADDOPR
    | MULTOPR
    | LITERAL
    | LPARENT
    | RPARENT
    | NOT

val string_of_term =
    fn PROGRAM => "PROGRAM"
    | IDENT => "IDENT"
    | GLOBAL => "GLOBAL"
    | DO => "DO"
    | ENDPROGRAM => "ENDPROGRAM"
    | CHANGEMODE => "CHANGEMODE"
    | FUN => "FUN"
    | RETURNS => "RETURNS"
    | LOCAL => "LOCAL"
    | ENDFUN => "ENDFUN"
    | PROC => "PROC"
    | ENDPROC => "ENDPROC"
    | COMMA => "COMMA"
    | FLOWMODE => "FLOWMODE"
    | SEMICOLON => "SEMICOLON"
    | LPAREN => "LPAREN"
    | RPAREN => "RPAREN"
    | MECHMODE => "MECHMODE"
    | COLON => "COLON"
    | INT => "INT"
    | INT32 => "INT32"
    | INT1024 => "INT1024"
    | BOOL => "BOOL"
    | SKIP => "SKIP"
    | BECOMES => "BECOMES"
    | IF => "IF"
    | THEN => "THEN"
    | ELSE => "ELSE"
    | ENDIF => "ENDIF"
    | WHILE => "WHILE"
    | ENDWHILE => "ENDWHILE"
    | CALL => "CALL"
    | DEBUGIN => "DEBUGIN"
    | DEBUGOUT => "DEBUGOUT"
    | INIT => "INIT"
    | CHAR => "CHAR"
    | NUMBER => "NUMBER"
    | LEBRKT => "LEBRKT"
    | REBRKT => "REBRKT"
    | DOT => "DOT"
    | LENGTH => "LENGTH"
    | BOOLOPR => "BOOLOPR"
    | RELOPR => "RELOPR"
    | ADDOPR => "ADDOPR"
    | MULTOPR => "MULTOPR"
    | LITERAL => "LITERAL"
    | LPARENT => "LPARENT"
    | RPARENT => "RPARENT"
    | NOT => "NOT"

val string_of_gramsym = (string_of_term, string_of_nonterm)
local
  open FixFoxi.FixFoxiCore
in
val productions = [
    (program,
        [[T PROGRAM, T IDENT, N progParamList, N opt0, T DO, N cpsCmd, T ENDPROGRAM]]),
    (decl,
        [[N stoDecl]
        ,[N funDecl]
        ,[N procDecl]]),
    (stoDecl,
        [[N opt1, N typedIdent]]),
    (funDecl,
        [[T FUN, T IDENT, N paramList, T RETURNS, N stoDecl, N opt2, N opt3, T DO, N cpsCmd, T ENDFUN]]),
    (procDecl,
        [[T PROC, T IDENT, N paramList, N opt4, N opt5, T DO, N cpsCmd, T ENDPROC]]),
    (globImps,
        [[N globImp, N rep0]]),
    (globImp,
        [[N opt6, N opt7, T IDENT]]),
    (cpsDecl,
        [[N decl, N rep1]]),
    (cpsStoDecl,
        [[N stoDecl, N rep2]]),
    (progParamList,
        [[T LPAREN, N opt8, T RPAREN]]),
    (progParam,
        [[N opt9, N opt10, N typedIdent]]),
    (paramList,
        [[T LPAREN, N opt11, T RPAREN]]),
    (param,
        [[N opt12, N opt13, N opt14, N typedIdent]]),
    (typedIdent,
        [[T IDENT, T COLON, N atomtype]]),
    (atomtype,
        [[N joice3, N array]]),
    (cmd,
        [[T SKIP]
        ,[N expr, T BECOMES, N expr]
        ,[T IF, N expr, T THEN, N cpsCmd, T ELSE, N cpsCmd, T ENDIF]
        ,[T WHILE, N expr, T DO, N cpsCmd, T ENDWHILE]
        ,[T CALL, T IDENT, N exprList, N opt15]
        ,[T DEBUGIN, N expr]
        ,[T DEBUGOUT, N expr]]),
    (cpsCmd,
        [[N cmd, N rep3]]),
    (globInits,
        [[T INIT, N idents]]),
    (idents,
        [[N ident, N rep4]]),
    (ident,
        [[T CHAR, N rep5, N joice6]]),
    (array,
        [[T LEBRKT, N opt16, T REBRKT]]),
    (arraylen,
        [[T DOT, T LENGTH]]),
    (expr,
        [[N term1, N rep6]]),
    (term1,
        [[N term2, N opt17]]),
    (term2,
        [[N term3, N rep7]]),
    (term3,
        [[N factor, N rep8]]),
    (factor,
        [[T LITERAL]
        ,[T IDENT, N opt18]
        ,[N monadicOpr, N factor]
        ,[T LPARENT, N expr, T RPARENT]]),
    (exprList,
        [[T LPARENT, N opt19, T RPARENT]]),
    (monadicOpr,
        [[T NOT]
        ,[T ADDOPR]]),
    (opt0,
        [[T GLOBAL, N cpsDecl]
        ,[]]),
    (opt1,
        [[T CHANGEMODE]
        ,[]]),
    (opt2,
        [[T GLOBAL, N globImps]
        ,[]]),
    (opt3,
        [[T LOCAL, N cpsStoDecl]
        ,[]]),
    (opt4,
        [[T GLOBAL, N globImps]
        ,[]]),
    (opt5,
        [[T LOCAL, N cpsStoDecl]
        ,[]]),
    (rep0,
        [[T COMMA, N globImp, N rep0]
        ,[]]),
    (opt6,
        [[T FLOWMODE]
        ,[]]),
    (opt7,
        [[T CHANGEMODE]
        ,[]]),
    (rep1,
        [[T SEMICOLON, N decl, N rep1]
        ,[]]),
    (rep2,
        [[T SEMICOLON, N stoDecl, N rep2]
        ,[]]),
    (opt8,
        [[N progParam, N rep9]
        ,[]]),
    (opt9,
        [[T FLOWMODE]
        ,[]]),
    (opt10,
        [[T CHANGEMODE]
        ,[]]),
    (opt11,
        [[N param, N rep10]
        ,[]]),
    (opt12,
        [[T FLOWMODE]
        ,[]]),
    (opt13,
        [[T MECHMODE]
        ,[]]),
    (opt14,
        [[T CHANGEMODE]
        ,[]]),
    (joice3,
        [[T INT]
        ,[T INT32]
        ,[T INT1024]
        ,[T BOOL]]),
    (opt15,
        [[N globInits]
        ,[]]),
    (rep3,
        [[T SEMICOLON, N cmd, N rep3]
        ,[]]),
    (rep4,
        [[T COMMA, N ident, N rep4]
        ,[]]),
    (rep5,
        [[N joice11, N rep5]
        ,[]]),
    (joice6,
        [[N array]
        ,[N arraylen]]),
    (opt16,
        [[N expr]
        ,[]]),
    (rep6,
        [[T BOOLOPR, N term1, N rep6]
        ,[]]),
    (opt17,
        [[T RELOPR, N term2]
        ,[]]),
    (rep7,
        [[T ADDOPR, N term3, N rep7]
        ,[]]),
    (rep8,
        [[T MULTOPR, N factor, N rep8]
        ,[]]),
    (opt18,
        [[N joice11]
        ,[]]),
    (opt19,
        [[N expr, N rep11]
        ,[]]),
    (rep9,
        [[T COMMA, N progParam, N rep9]
        ,[]]),
    (rep10,
        [[T COMMA, N param, N rep10]
        ,[]]),
    (joice11,
        [[T CHAR]
        ,[T NUMBER]]),
    (joice11,
        [[T INIT]
        ,[N exprList]]),
    (rep11,
        [[T COMMA, N expr, N rep11]
        ,[]])
]

val S = program
val result = fix_foxi productions S string_of_gramsym
end (* local *)
