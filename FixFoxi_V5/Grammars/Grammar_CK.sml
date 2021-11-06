
datatype term
    = BOOL
    | CALL
    | CONST
    | COPY
    | DEBUGIN
    | DEBUGOUT
    | DIVE
    | DIVF
    | DIVT
    | DO
    | ELSE
    | ENDFUN
    | ENDIF
    | ENDPROC
    | ENDPROGRAM
    | ENDWHILE
    | FALSE
    | FUN
    | GLOBAL
    | IF
    | IN
    | INIT
    | INOUT
    | INT1024
    | INT32
    | INT64
    | LOCAL
    | MODE
    | MODF
    | MODT
    | NOT
    | OUT
    | PROC
    | PROGRAM
    | REF
    | RETURNS
    | SKIP
    | THEN
    | TRUE
    | VAR
    | WHILE
    | FALSE
    | TRUE
    | PROGRAM
    | DO
    | ENDPROGRAM
    | IN
    | OUT
    | INOUT
    | COPY
    | REF
    | CONST
    | VAR
    | BOOL
    | INT32
    | INT64
    | INT1024
    | FUN
    | GLOBAL
    | DO
    | ENDPROC
    | PROC
    | GLOBAL
    | DO
    | ENDPROC
    | SKIP
    | IF
    | ELSE
    | ENDIF
    | WHILE
    | ENDWHILE
    | CALL
    | DEBUGIN
    | DEBUGOUT
    | INIT
    | DIVE
    | MODE
    | DIVF
    | MODF
    | DIVT
    | MODT
    | NOT


val string_of_term =
    fn BOOL => "BOOL"
    | CALL => "CALL"
    | CONST => "CONST"
    | COPY => "COPY"
    | DEBUGIN => "DEBUGIN"
    | DEBUGOUT => "DEBUGOUT"
    | DIVE => "DIVE"
    | DIVF => "DIVF"
    | DIVT => "DIVT"
    | DO => "DO"
    | ELSE => "ELSE"
    | ENDFUN => "ENDFUN"
    | ENDIF => "ENDIF"
    | ENDPROC => "ENDPROC"
    | ENDPROGRAM => "ENDPROGRAM"
    | ENDWHILE => "ENDWHILE"
    | FALSE => "FALSE"
    | FUN => "FUN"
    | GLOBAL => "GLOBAL"
    | IF => "IF"
    | IN => "IN"
    | INIT => "INIT"
    | INOUT => "INOUT"
    | INT1024 => "INT1024"
    | INT32 => "INT32"
    | INT64 => "INT64"
    | LOCAL => "LOCAL"
    | MODE => "MODE"
    | MODF => "MODF"
    | MODT => "MODT"
    | NOT => "NOT"
    | OUT => "OUT"
    | PROC => "PROC"
    | PROGRAM => "PROGRAM"
    | REF => "REF"
    | RETURNS => "RETURNS"
    | SKIP => "SKIP"
    | THEN => "THEN"
    | TRUE => "TRUE"
    | VAR => "VAR"
    | WHILE => "WHILE"
    | FALSE => "FALSE"
    | TRUE => "TRUE"
    | PROGRAM => "PROGRAM"
    | DO => "DO"
    | ENDPROGRAM => "ENDPROGRAM"
    | IN => "IN"
    | OUT => "OUT"
    | INOUT => "INOUT"
    | COPY => "COPY"
    | REF => "REF"
    | CONST => "CONST"
    | VAR => "VAR"
    | BOOL => "BOOL"
    | INT32 => "INT32"
    | INT64 => "INT64"
    | INT1024 => "INT1024"
    | FUN => "FUN"
    | GLOBAL => "GLOBAL"
    | DO => "DO"
    | ENDPROC => "ENDPROC"
    | PROC => "PROC"
    | GLOBAL => "GLOBAL"
    | DO => "DO"
    | ENDPROC => "ENDPROC"
    | SKIP => "SKIP"
    | IF => "IF"
    | ELSE => "ELSE"
    | ENDIF => "ENDIF"
    | WHILE => "WHILE"
    | ENDWHILE => "ENDWHILE"
    | CALL => "CALL"
    | DEBUGIN => "DEBUGIN"
    | DEBUGOUT => "DEBUGOUT"
    | INIT => "INIT"
    | DIVE => "DIVE"
    | MODE => "MODE"
    | DIVF => "DIVF"
    | MODF => "MODF"
    | DIVT => "DIVT"
    | MODT => "MODT"
    | NOT => "NOT"


datatype nonterm
    = DIGIT
    | LOWERCASE
    | UPPERCASE
    | LETTER
    | SPECIAL
    | SPACE
    | PRINTABLE
    | LINEFEED
    | CARRIET
    | NEWLINE
    | WHITEBASE
    | COMMENT
    | WHITESPACE
    | RESERVEDID
    | SYMBOL
    | BOOLLIT
    | INTLIT
    | LITERAL
    | IDENT
    | LEXEME
    | LEXEMES
    | PROGRAM
    | FLOWMODE
    | MECHMODE
    | CHANGEMODE
    | TYPE
    | PRIMTYPE
    | BOOLTYPE
    | INTTYPE
    | ARRAYTYPE
    | PROGPARAMLIST
    | PROGPARAM
    | PARAMLIST
    | PARAM
    | TYPEDIDENT
    | DECL
    | STODECL
    | FUNDECL
    | PROCDECL
    | GLOBIMPS
    | GLOBIMP
    | CPSDECL
    | CPSSTODECL
    | CMD
    | CPSCMD
    | GLOBINITS
    | EXPR
    | TERM1
    | TERM2
    | TERM3
    | FACTOR
    | EXPRLIST
    | RELOPR
    | ADDOPR
    | MULTOPR
    | DIVOPR
    | MONOPR

val string_of_nonterm =
    fn DIGIT => "DIGIT"
    | LOWERCASE => "LOWERCASE"
    | UPPERCASE => "UPPERCASE"
    | LETTER => "LETTER"
    | SPECIAL => "SPECIAL"
    | SPACE => "SPACE"
    | PRINTABLE => "PRINTABLE"
    | LINEFEED => "LINEFEED"
    | CARRIET => "CARRIET"
    | NEWLINE => "NEWLINE"
    | WHITEBASE => "WHITEBASE"
    | COMMENT => "COMMENT"
    | WHITESPACE => "WHITESPACE"
    | RESERVEDID => "RESERVEDID"
    | SYMBOL => "SYMBOL"
    | BOOLLIT => "BOOLLIT"
    | INTLIT => "INTLIT"
    | LITERAL => "LITERAL"
    | IDENT => "IDENT"
    | LEXEME => "LEXEME"
    | LEXEMES => "LEXEMES"
    | PROGRAM => "PROGRAM"
    | FLOWMODE => "FLOWMODE"
    | MECHMODE => "MECHMODE"
    | CHANGEMODE => "CHANGEMODE"
    | TYPE => "TYPE"
    | PRIMTYPE => "PRIMTYPE"
    | BOOLTYPE => "BOOLTYPE"
    | INTTYPE => "INTTYPE"
    | ARRAYTYPE => "ARRAYTYPE"
    | PROGPARAMLIST => "PROGPARAMLIST"
    | PROGPARAM => "PROGPARAM"
    | PARAMLIST => "PARAMLIST"
    | PARAM => "PARAM"
    | TYPEDIDENT => "TYPEDIDENT"
    | DECL => "DECL"
    | STODECL => "STODECL"
    | FUNDECL => "FUNDECL"
    | PROCDECL => "PROCDECL"
    | GLOBIMPS => "GLOBIMPS"
    | GLOBIMP => "GLOBIMP"
    | CPSDECL => "CPSDECL"
    | CPSSTODECL => "CPSSTODECL"
    | CMD => "CMD"
    | CPSCMD => "CPSCMD"
    | GLOBINITS => "GLOBINITS"
    | EXPR => "EXPR"
    | TERM1 => "TERM1"
    | TERM2 => "TERM2"
    | TERM3 => "TERM3"
    | FACTOR => "FACTOR"
    | EXPRLIST => "EXPRLIST"
    | RELOPR => "RELOPR"
    | ADDOPR => "ADDOPR"
    | MULTOPR => "MULTOPR"
    | DIVOPR => "DIVOPR"
    | MONOPR => "MONOPR"



val string_of_gramsym = (string_of_term, string_of_nonterm)

local
  open FixFoxi.FixFoxiCore
in

val productions =
[
(*
    expr   ::= term3 (ADDOPR term3)*
    term3  ::= factor (MULTOPR factor)*
    factor ::= IDENT
            |  LPAREN expr RPAREN
*)
(expr,
    [[N term3, N repADDOPRterm3]]),
(repADDOPRterm3,
    [[T ADDOPR, N term3, N repADDOPRterm3],
     []]),
(term3,
    [[N factor, N repMULTOPRfactor]]),
(repMULTOPRfactor,
    [[T MULTOPR, N factor, N repMULTOPRfactor],
     []]),
(factor,
    [[T IDENT],
     [T LPAREN, N expr, T RPAREN]])
]

val S = expr

val result = fix_foxi productions S string_of_gramsym

end (* local *)
