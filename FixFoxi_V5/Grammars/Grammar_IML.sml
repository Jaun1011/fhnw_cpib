(*
file:	Grammar_IML.grammar
time:	2018-11-06 21:33:25.174319
*)

datatype term
  = ADDOPR
  | ATOMTYPE
  | BECOMES
  | BOOLOPR
  | CALL
  | CHANGEMODE
  | COLON
  | COMMA
  | DEBUGIN
  | DEBUGOUT
  | DO
  | ELSE
  | ENDFUN
  | ENDIF
  | ENDPROC
  | ENDPROGRAM
  | ENDWHILE
  | FLOWMODE
  | FUN
  | GLOBAL
  | IDENT
  | IF
  | INIT
  | LITERAL
  | LOCAL
  | LPAREN
  | MECHMODE
  | MULTOPR
  | NOT
  | PROC
  | PROGRAM
  | RELOPR
  | RETURNS
  | RPAREN
  | SEMICOLON
  | SKIP
  | THEN
  | WHILE

val string_of_term =
  fn ADDOPR => "ADDOPR"
   | ATOMTYPE => "ATOMTYPE"
   | BECOMES => "BECOMES"
   | BOOLOPR => "BOOLOPR"
   | CALL => "CALL"
   | CHANGEMODE => "CHANGEMODE"
   | COLON => "COLON"
   | COMMA => "COMMA"
   | DEBUGIN => "DEBUGIN"
   | DEBUGOUT => "DEBUGOUT"
   | DO => "DO"
   | ELSE => "ELSE"
   | ENDFUN => "ENDFUN"
   | ENDIF => "ENDIF"
   | ENDPROC => "ENDPROC"
   | ENDPROGRAM => "ENDPROGRAM"
   | ENDWHILE => "ENDWHILE"
   | FLOWMODE => "FLOWMODE"
   | FUN => "FUN"
   | GLOBAL => "GLOBAL"
   | IDENT => "IDENT"
   | IF => "IF"
   | INIT => "INIT"
   | LITERAL => "LITERAL"
   | LOCAL => "LOCAL"
   | LPAREN => "LPAREN"
   | MECHMODE => "MECHMODE"
   | MULTOPR => "MULTOPR"
   | NOT => "NOT"
   | PROC => "PROC"
   | PROGRAM => "PROGRAM"
   | RELOPR => "RELOPR"
   | RETURNS => "RETURNS"
   | RPAREN => "RPAREN"
   | SEMICOLON => "SEMICOLON"
   | SKIP => "SKIP"
   | THEN => "THEN"
   | WHILE => "WHILE"

datatype nonterm
  = cmd
  | cpsCmd
  | cpsDecl
  | cpsStoDecl
  | decl
  | expr
  | exprList
  | factor
  | funDecl
  | globImp
  | globImps
  | globInits
  | idents
  | monadicOpr
  | opt_CHANGEMODE
  | opt_FLOWMODE
  | opt_GLOBALcpsDecl
  | opt_GLOBALglobImps
  | opt_INITexprList
  | opt_LOCALcpsStoDecl
  | opt_MECHMODE
  | opt_RELOPRterm2
  | opt_exprrep_COMMAexpr
  | opt_globInits
  | opt_paramrep_COMMAparam
  | opt_progParamrep_COMMAprogParam
  | param
  | paramList
  | procDecl
  | progParam
  | progParamList
  | program
  | rep_ADDOPRterm3
  | rep_BOOLOPRterm1
  | rep_COMMAIDENT
  | rep_COMMAexpr
  | rep_COMMAglobImp
  | rep_COMMAparam
  | rep_COMMAprogParam
  | rep_MULTOPRfactor
  | rep_SEMICOLONcmd
  | rep_SEMICOLONdecl
  | rep_SEMICOLONstoDecl
  | stoDecl
  | term1
  | term2
  | term3
  | typedIdent

val string_of_nonterm =
  fn cmd => "cmd"
   | cpsCmd => "cpsCmd"
   | cpsDecl => "cpsDecl"
   | cpsStoDecl => "cpsStoDecl"
   | decl => "decl"
   | expr => "expr"
   | exprList => "exprList"
   | factor => "factor"
   | funDecl => "funDecl"
   | globImp => "globImp"
   | globImps => "globImps"
   | globInits => "globInits"
   | idents => "idents"
   | monadicOpr => "monadicOpr"
   | opt_CHANGEMODE => "opt_CHANGEMODE"
   | opt_FLOWMODE => "opt_FLOWMODE"
   | opt_GLOBALcpsDecl => "opt_GLOBALcpsDecl"
   | opt_GLOBALglobImps => "opt_GLOBALglobImps"
   | opt_INITexprList => "opt_INITexprList"
   | opt_LOCALcpsStoDecl => "opt_LOCALcpsStoDecl"
   | opt_MECHMODE => "opt_MECHMODE"
   | opt_RELOPRterm2 => "opt_RELOPRterm2"
   | opt_exprrep_COMMAexpr => "opt_exprrep_COMMAexpr"
   | opt_globInits => "opt_globInits"
   | opt_paramrep_COMMAparam => "opt_paramrep_COMMAparam"
   | opt_progParamrep_COMMAprogParam => "opt_progParamrep_COMMAprogParam"
   | param => "param"
   | paramList => "paramList"
   | procDecl => "procDecl"
   | progParam => "progParam"
   | progParamList => "progParamList"
   | program => "program"
   | rep_ADDOPRterm3 => "rep_ADDOPRterm3"
   | rep_BOOLOPRterm1 => "rep_BOOLOPRterm1"
   | rep_COMMAIDENT => "rep_COMMAIDENT"
   | rep_COMMAexpr => "rep_COMMAexpr"
   | rep_COMMAglobImp => "rep_COMMAglobImp"
   | rep_COMMAparam => "rep_COMMAparam"
   | rep_COMMAprogParam => "rep_COMMAprogParam"
   | rep_MULTOPRfactor => "rep_MULTOPRfactor"
   | rep_SEMICOLONcmd => "rep_SEMICOLONcmd"
   | rep_SEMICOLONdecl => "rep_SEMICOLONdecl"
   | rep_SEMICOLONstoDecl => "rep_SEMICOLONstoDecl"
   | stoDecl => "stoDecl"
   | term1 => "term1"
   | term2 => "term2"
   | term3 => "term3"
   | typedIdent => "typedIdent"


val string_of_gramsym = (string_of_term, string_of_nonterm)

local
  open FixFoxi.FixFoxiCore
in


val productions =
[
  (opt_GLOBALcpsDecl, [
    [T GLOBAL, N cpsDecl],
    []]),
  (program, [
    [T PROGRAM, T IDENT, N progParamList, N opt_GLOBALcpsDecl, T DO, N cpsCmd, T ENDPROGRAM]]),
  (decl, [
    [N stoDecl],
    [N funDecl],
    [N procDecl]]),
  (opt_CHANGEMODE, [
    [T CHANGEMODE],
    []]),
  (stoDecl, [
    [N opt_CHANGEMODE, N typedIdent]]),
  (opt_GLOBALglobImps, [
    [T GLOBAL, N globImps],
    []]),
  (opt_LOCALcpsStoDecl, [
    [T LOCAL, N cpsStoDecl],
    []]),
  (funDecl, [
    [T FUN, T IDENT, N paramList, T RETURNS, N stoDecl, N opt_GLOBALglobImps, N opt_LOCALcpsStoDecl, T DO, N cpsCmd, T ENDFUN]
  ]),
  (procDecl, [
    [T PROC, T IDENT, N paramList, N opt_GLOBALglobImps, N opt_LOCALcpsStoDecl, T DO, N cpsCmd, T ENDPROC]]),
  (rep_COMMAglobImp, [
    [T COMMA, N globImp, N rep_COMMAglobImp],
    []]),
  (globImps, [
    [N globImp, N rep_COMMAglobImp]]),
  (opt_FLOWMODE, [
    [T FLOWMODE],
    []]),
  (globImp, [
    [N opt_FLOWMODE, N opt_CHANGEMODE, T IDENT]]),
  (rep_SEMICOLONdecl, [
    [T SEMICOLON, N decl, N rep_SEMICOLONdecl],
    []]),
  (cpsDecl, [
    [N decl, N rep_SEMICOLONdecl]]),
  (rep_SEMICOLONstoDecl, [
    [T SEMICOLON, N stoDecl, N rep_SEMICOLONstoDecl],
    []]),
  (cpsStoDecl, [
    [N stoDecl, N rep_SEMICOLONstoDecl]]),
  (rep_COMMAprogParam, [
    [T COMMA, N progParam, N rep_COMMAprogParam],
    []]),
  (opt_progParamrep_COMMAprogParam, [
    [N progParam, N rep_COMMAprogParam],
    []]),
  (progParamList, [
    [T LPAREN, N opt_progParamrep_COMMAprogParam, T RPAREN]]),
  (progParam, [
    [N opt_FLOWMODE, N opt_CHANGEMODE, N typedIdent]]),
  (rep_COMMAparam, [
    [T COMMA, N param, N rep_COMMAparam],
    []]),
  (opt_paramrep_COMMAparam, [
    [N param, N rep_COMMAparam],
    []]),
  (paramList, [
    [T LPAREN, N opt_paramrep_COMMAparam, T RPAREN]]),
  (opt_MECHMODE, [
    [T MECHMODE],
    []]),
  (param, [
    [N opt_FLOWMODE, N opt_MECHMODE, N opt_CHANGEMODE, N typedIdent]]),
  (typedIdent, [[T IDENT, T COLON, T ATOMTYPE]]),
  (opt_globInits, [[N globInits], []]),
  (cmd, [ 
    [T SKIP],
    [N expr, T BECOMES, N expr],
    [T IF, N expr, T THEN, N cpsCmd, T ELSE, N cpsCmd, T ENDIF],
    [T WHILE, N expr, T DO, N cpsCmd, T ENDWHILE],
    [T CALL, T IDENT, N exprList, N opt_globInits],
    [T DEBUGIN, N expr],
    [T DEBUGOUT, N expr]]),
  (rep_SEMICOLONcmd, [
    [T SEMICOLON, N cmd, N rep_SEMICOLONcmd],
    []]),
  (cpsCmd, [
    [N cmd, N rep_SEMICOLONcmd]]),
  (globInits, [
    [T INIT, N idents]]),
  (rep_COMMAIDENT, [
    [T COMMA, T IDENT, N rep_COMMAIDENT],
    []]),
  (idents, [
    [T IDENT, N rep_COMMAIDENT]]),
  (rep_BOOLOPRterm1, [
    [T BOOLOPR, N term1, N rep_BOOLOPRterm1],
    []]),
  (expr, [
    [N term1, N rep_BOOLOPRterm1]]),
  (opt_RELOPRterm2, [
    [T RELOPR, N term2],
    []]),
  (term1, [
    [N term2, N opt_RELOPRterm2]]),
  (rep_ADDOPRterm3, [
    [T ADDOPR, N term3, N rep_ADDOPRterm3],
    []]),
  (term2, [[N term3, N rep_ADDOPRterm3]]),

  (rep_MULTOPRfactor, [
    [T MULTOPR, N factor, N rep_MULTOPRfactor],
    []]),
  (term3, [
    [N factor, N rep_MULTOPRfactor]]),
  (opt_INITexprList, [
    [T INIT],
    [N exprList],
    []]),
  (factor, [
    [T LITERAL],
    [T IDENT, N opt_INITexprList],
    [N monadicOpr, N factor],
    [T LPAREN, N expr, T RPAREN]]),
  (rep_COMMAexpr, [
    [T COMMA, N expr, N rep_COMMAexpr],
    []]),
  (opt_exprrep_COMMAexpr, [
    [N expr, N rep_COMMAexpr],
    []]),
  (exprList, [
    [T LPAREN, N opt_exprrep_COMMAexpr, T RPAREN]]),
  (monadicOpr, [
    [T NOT],
    [T ADDOPR]])
]

val S = program

val result = fix_foxi productions S string_of_gramsym

end (* local *)

