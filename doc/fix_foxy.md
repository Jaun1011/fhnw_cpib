[opening Grammar_CK2.sml]
Grammar_CK2.sml:414.1-414.54 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
Warning: unreachable nonterminals:
<chars>
<space>
<printable>
<newline>
<whitebase>
<comment>
<whitespace>
<special>
<program>
<flowmode>
<mechmode>
<changemode>
<booltype>
<inttype>
<primtype>
<arraytype>
<typedef>
<progParamList>
<progParamListItem>
<progParam>
<paramList>
<paramListItem>
<param>
<typedIdent>
<decl>
<stoDecl>
<funDecl>
<procDecl>
<globImps>
<globImp>
<cpsDecl>
<cpsDeclItem>
<cpsStoDecl>
<cpsStoDeclItem>
<cmd>
<cpsCmd>
<cpsCmdItem>
<globInits>
<globInitsItem>
<multopr>
<divopr>
Warning: grammar not LL1:
<literal>
  terminal TRUE
    <boollit>
    <intlit>
<ident>
  terminal HIGHCHAR
    <letter>
    <ident> <digit> <reservedid>
    <ident> <letter> <reservedid>
  terminal LOWCHAR
    <letter>
    <ident> <digit> <reservedid>
    <ident> <letter> <reservedid>
<term1>
  terminal LPARENT
    <term2> <relopr> <term1>
    <term2>
  terminal MINUSOPR
    <term2> <relopr> <term1>
    <term2>
  terminal PLUSOPR
    <term2> <relopr> <term1>
    <term2>
  terminal NOT
    <term2> <relopr> <term1>
    <term2>
  terminal HIGHCHAR
    <term2> <relopr> <term1>
    <term2>
  terminal LOWCHAR
    <term2> <relopr> <term1>
    <term2>
  terminal NUMBER
    <term2> <relopr> <term1>
    <term2>
  terminal TRUE
    <term2> <relopr> <term1>
    <term2>
  terminal FALSE
    <term2> <relopr> <term1>
    <term2>
<term2>
  terminal LPARENT
    <term3> <addopr> <term2>
    <term3>
  terminal MINUSOPR
    <term3> <addopr> <term2>
    <term3>
  terminal PLUSOPR
    <term3> <addopr> <term2>
    <term3>
  terminal NOT
    <term3> <addopr> <term2>
    <term3>
  terminal HIGHCHAR
    <term3> <addopr> <term2>
    <term3>
  terminal LOWCHAR
    <term3> <addopr> <term2>
    <term3>
  terminal NUMBER
    <term3> <addopr> <term2>
    <term3>
  terminal TRUE
    <term3> <addopr> <term2>
    <term3>
  terminal FALSE
    <term3> <addopr> <term2>
    <term3>
<term3>
  terminal LPARENT
    <factor> <addopr> <term3>
    <factor>
  terminal MINUSOPR
    <factor> <addopr> <term3>
    <factor>
  terminal PLUSOPR
    <factor> <addopr> <term3>
    <factor>
  terminal NOT
    <factor> <addopr> <term3>
    <factor>
  terminal HIGHCHAR
    <factor> <addopr> <term3>
    <factor>
  terminal LOWCHAR
    <factor> <addopr> <term3>
    <factor>
  terminal NUMBER
    <factor> <addopr> <term3>
    <factor>
  terminal TRUE
    <factor> <addopr> <term3>
    <factor>
  terminal FALSE
    <factor> <addopr> <term3>
    <factor>
<factor>
  terminal HIGHCHAR
    <ident>
    <ident> IDENT
    <ident> <exprList>
  terminal LOWCHAR
    <ident>
    <ident> IDENT
    <ident> <exprList>
datatype term
  = AND
  | ASSIGN
  | BACKSLASH
  | BOOL
  | BRACKET_EDGE_L
  | BRACKET_EDGE_R
  | CALL
  | COMMA
  | CONST
  | COPY
  | DEBUGIN
  | DEBUGOUT
  | DIVE
  | DIVF
  | DIVOPR
  | DIVT
  | DO
  | DPOINT
  | ELSE
  | ENDFUN
  | ENDIF
  | ENDPROC
  | ENDPROGRAM
  | ENDWHILE
  | EQUAL
  | FALSE
  | FUN
  | GLOBAL
  | GREATER
  | GREATEREQUAL
  | HIGHCHAR
  | HIGHCOMMA
  | IDENT
  | IF
  | IN
  | INIT
  | INOUT
  | INT1024
  | INT32
  | INT64
  | LESS
  | LESSEQUAL
  | LOCAL
  | LOWCHAR
  | LPARENT
  | MINUSOPR
  | MODE
  | MODF
  | MODT
  | MULTOPR
  | NEWLINE
  | NOT
  | NOTEQUAL
  | NUMBER
  | OR
  | OUT
  | PLUSOPR
  | POINT
  | PROC
  | PROGRAM
  | REF
  | RETURNS
  | RPARENT
  | SEMICOLON
  | SKIP
  | THEN
  | TRUE
  | VAR
  | WHILE
  | WHITESPACE
val string_of_term = fn : term -> string

datatype nonterm
  = addopr
  | arraytype
  | boollit
  | booltype
  | carriet
  | changemode
  | chars
  | cmd
  | comment
  | cpsCmd
  | cpsCmdItem
  | cpsDecl
  | cpsDeclItem
  | cpsStoDecl
  | cpsStoDeclItem
  | decl
  | digit
  | divopr
  | expr
  | exprItem
  | exprList
  | exprListItem
  | factor
  | flowmode
  | funDecl
  | globImp
  | globImps
  | globInits
  | globInitsItem
  | ident
  | intlit
  | inttype
  | letter
  | lexeme
  | lexemes
  | linefeed
  | literal
  | lowercase
  | mechmode
  | monopr
  | multopr
  | newline
  | param
  | paramList
  | paramListItem
  | primtype
  | printable
  | procDecl
  | progParam
  | progParamList
  | progParamListItem
  | program
  | relopr
  | reservedid
  | space
  | special
  | stoDecl
  | symbol
  | term1
  | term2
  | term3
  | typedIdent
  | typedef
  | uppercase
  | whitebase
  | whitespace
val string_of_nonterm = fn : nonterm -> string
val string_of_gramsym = (fn,fn) : (term -> string) * (nonterm -> string)
val productions =
  [(digit,[[T NUMBER]]),(lowercase,[[T LOWCHAR]]),(uppercase,[[T HIGHCHAR]]),
   (letter,[[N lowercase],[N uppercase]]),(chars,[[N letter],[N digit]]),
   (space,[[T WHITESPACE]]),(printable,[[N digit],[N letter],[N space]]),
   (newline,[[T NEWLINE]]),(whitebase,[[N space],[N newline]]),
   (comment,[[T BACKSLASH,T BACKSLASH,N printable]]),
   (whitespace,[[N whitebase],[N comment]]),
   (reservedid,
    [[T BOOL],[T CALL],[T CONST],[T COPY],[T DEBUGIN],[T DEBUGOUT],[T DIVE],
     [T DIVF],[T DIVT],[T DO],[T ELSE],[T ENDFUN],[T ENDIF],[T ENDPROC],
     [T ENDPROGRAM],[T ENDWHILE],[T FALSE],[T FUN],[T GLOBAL],[T IF],[T IN],
     [T INIT],[T INOUT],[T INT1024],...]),
   (special,
    [[T LPARENT],[T COMMA],[T RPARENT],[T POINT],[T SEMICOLON],[T ASSIGN],
     [T OR],[T AND],[T EQUAL],[T NOTEQUAL],[T LESS],[T GREATER],[T LESSEQUAL],
     [T GREATEREQUAL],[T PLUSOPR],[T MINUSOPR],[T MULTOPR],[T DIVOPR]]),
   (boollit,[[T FALSE],[T TRUE]]),(intlit,[[N digit],[T TRUE]]),
   (literal,[[N boollit],[N intlit]]),
   (ident,
    [[N letter],[N ident,N digit,N reservedid],
     [N ident,N letter,N reservedid]]),
   (program,
    [[T PROGRAM,N ident,T DO,N cpsCmd,T ENDPROGRAM],
     [T PROGRAM,N ident,T GLOBAL,N cpsDecl,T DO,N cpsCmd,T ENDPROGRAM]]),
   (flowmode,[[T IN],[T OUT],[T INOUT]]),(mechmode,[[T COPY],[T REF]]),
   (changemode,[[T CONST],[T VAR]]),(booltype,[[T BOOL]]),
   (inttype,[[T INT32],[T INT64],[T INT1024]]),
   (primtype,[[N booltype],[N inttype]]),...]
  : (nonterm * (term,nonterm) FixFoxiCore.grammar_symbol list list) list
val S = expr : nonterm
val result =
  ({empty_RHSs=[],multiples_LHS=[],unproductives=[]},
   SOME
     ({grammar_not_LL1=[(literal,[(U TRUE,[[#],[#]])]),
                        (ident,
                         [(U HIGHCHAR,[[#],[#,#,#],[#,#,#]]),
                          (U LOWCHAR,[[#],[#,#,#],[#,#,#]])]),
                        (term1,
                         [(U LPARENT,[[#,#,#],[#]]),(U MINUSOPR,[[#,#,#],[#]]),
                          (U PLUSOPR,[[#,#,#],[#]]),(U NOT,[[#,#,#],[#]]),
                          (U HIGHCHAR,[[#,#,#],[#]]),(U LOWCHAR,[[#,#,#],[#]]),
                          (U NUMBER,[[#,#,#],[#]]),(U TRUE,[[#,#,#],[#]]),
                          (U FALSE,[[#,#,#],[#]])]),
                        (term2,
                         [(U LPARENT,[[#,#,#],[#]]),(U MINUSOPR,[[#,#,#],[#]]),
                          (U PLUSOPR,[[#,#,#],[#]]),(U NOT,[[#,#,#],[#]]),
                          (U HIGHCHAR,[[#,#,#],[#]]),(U LOWCHAR,[[#,#,#],[#]]),
                          (U NUMBER,[[#,#,#],[#]]),(U TRUE,[[#,#,#],[#]]),
                          (U FALSE,[[#,#,#],[#]])]),
                        (term3,
                         [(U LPARENT,[[#,#,#],[#]]),(U MINUSOPR,[[#,#,#],[#]]),
                          (U PLUSOPR,[[#,#,#],[#]]),(U NOT,[[#,#,#],[#]]),
                          (U HIGHCHAR,[[#,#,#],[#]]),(U LOWCHAR,[[#,#,#],[#]]),
                          (U NUMBER,[[#,#,#],[#]]),(U TRUE,[[#,#,#],[#]]),
                          (U FALSE,[[#,#,#],[#]])]),
                        (factor,
                         [(U HIGHCHAR,[[#],[#,#],[#,#]]),
                          (U LOWCHAR,[[#],[#,#],[#,#]])])],
       unreachables=[chars,space,printable,newline,whitebase,comment,
                     whitespace,special,program,flowmode,mechmode,changemode,
                     booltype,inttype,primtype,arraytype,typedef,progParamList,
                     progParamListItem,progParam,paramList,paramListItem,param,
                     typedIdent,...]},
      {FIRST=[(digit,[NUMBER]),(lowercase,[LOWCHAR]),(uppercase,[HIGHCHAR]),
              (letter,[HIGHCHAR,LOWCHAR]),
              (reservedid,
               [WHILE,VAR,TRUE,THEN,SKIP,RETURNS,REF,PROGRAM,PROC,OUT,NOT,MODT,
                MODF,MODE,LOCAL,INT64,INT32,INT1024,INOUT,INIT,IN,IF,GLOBAL,
                FUN,...]),(boollit,[TRUE,FALSE]),(intlit,[TRUE,NUMBER]),
              (literal,[NUMBER,TRUE,FALSE]),(ident,[HIGHCHAR,LOWCHAR]),
              (expr,
               [LPARENT,MINUSOPR,PLUSOPR,NOT,HIGHCHAR,LOWCHAR,NUMBER,TRUE,
                FALSE]),(exprItem,[OR,AND]),
              (term1,
               [LPARENT,MINUSOPR,PLUSOPR,NOT,HIGHCHAR,LOWCHAR,NUMBER,TRUE,
                FALSE]),
              (term2,
               [LPARENT,MINUSOPR,PLUSOPR,NOT,HIGHCHAR,LOWCHAR,NUMBER,TRUE,
                FALSE]),
              (term3,
               [LPARENT,MINUSOPR,PLUSOPR,NOT,HIGHCHAR,LOWCHAR,NUMBER,TRUE,
                FALSE]),
              (factor,
               [LPARENT,MINUSOPR,PLUSOPR,NOT,HIGHCHAR,LOWCHAR,NUMBER,TRUE,
                FALSE]),(exprList,[LPARENT]),(exprListItem,[COMMA]),
              (relopr,[LESSEQUAL,LESS,GREATEREQUAL,GREATER,NOTEQUAL,EQUAL]),
              (addopr,[MINUSOPR,PLUSOPR]),(monopr,[MINUSOPR,PLUSOPR,NOT])],
       FOLLOW=[(digit,
                [U WHILE,U VAR,U TRUE,U THEN,U SKIP,U RETURNS,U REF,U PROGRAM,
                 U PROC,U OUT,U NOT,U MODT,U MODF,U MODE,U LOCAL,U INT64,
                 U INT32,U INT1024,U INOUT,U INIT,U IN,U IF,U GLOBAL,U FUN,
                 ...]),
               (lowercase,
                [U WHILE,U VAR,U TRUE,U THEN,U SKIP,U RETURNS,U REF,U PROGRAM,
                 U PROC,U OUT,U NOT,U MODT,U MODF,U MODE,U LOCAL,U INT64,
                 U INT32,U INT1024,U INOUT,U INIT,U IN,U IF,U GLOBAL,U FUN,
                 ...]),
               (uppercase,
                [U WHILE,U VAR,U TRUE,U THEN,U SKIP,U RETURNS,U REF,U PROGRAM,
                 U PROC,U OUT,U NOT,U MODT,U MODF,U MODE,U LOCAL,U INT64,
                 U INT32,U INT1024,U INOUT,U INIT,U IN,U IF,U GLOBAL,U FUN,
                 ...]),
               (letter,
                [U WHILE,U VAR,U TRUE,U THEN,U SKIP,U RETURNS,U REF,U PROGRAM,
                 U PROC,U OUT,U NOT,U MODT,U MODF,U MODE,U LOCAL,U INT64,
                 U INT32,U INT1024,U INOUT,U INIT,U IN,U IF,U GLOBAL,U FUN,
                 ...]),
               (reservedid,
                [U LPARENT,U IDENT,$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,
                 U LESS,U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL,U MINUSOPR,
                 U PLUSOPR,U HIGHCHAR,U LOWCHAR,U NUMBER]),
               (boollit,
                [$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,U LESS,
                 U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL,U MINUSOPR,
                 U PLUSOPR]),
               (intlit,
                [$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,U LESS,
                 U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL,U MINUSOPR,
                 U PLUSOPR]),
               (literal,
                [$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,U LESS,
                 U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL,U MINUSOPR,
                 U PLUSOPR]),
               (ident,
                [U LPARENT,U IDENT,$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,
                 U LESS,U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL,U MINUSOPR,
                 U PLUSOPR,U HIGHCHAR,U LOWCHAR,U NUMBER]),
               (expr,[$,U COMMA,U RPARENT]),(exprItem,[$,U COMMA,U RPARENT]),
               (term1,[$,U COMMA,U RPARENT,U OR,U AND]),
               (term2,
                [$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,U LESS,
                 U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL]),
               (term3,
                [$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,U LESS,
                 U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL,U MINUSOPR,
                 U PLUSOPR]),
               (factor,
                [$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,U LESS,
                 U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL,U MINUSOPR,
                 U PLUSOPR]),
               (exprList,
                [$,U COMMA,U RPARENT,U OR,U AND,U LESSEQUAL,U LESS,
                 U GREATEREQUAL,U GREATER,U NOTEQUAL,U EQUAL,U MINUSOPR,
                 U PLUSOPR]),(exprListItem,[U RPARENT]),
               (relopr,
                [U LPARENT,U MINUSOPR,U PLUSOPR,U NOT,U HIGHCHAR,U LOWCHAR,
                 U NUMBER,U TRUE,U FALSE]),
               (addopr,
                [U LPARENT,U MINUSOPR,U PLUSOPR,U NOT,U HIGHCHAR,U LOWCHAR,
                 U NUMBER,U TRUE,U FALSE]),
               (monopr,
                [U LPARENT,U MINUSOPR,U PLUSOPR,U NOT,U HIGHCHAR,U LOWCHAR,
                 U NUMBER,U TRUE,U FALSE])],
       MM=[(digit,[(U NUMBER,[[#]])]),(lowercase,[(U LOWCHAR,[[#]])]),
           (uppercase,[(U HIGHCHAR,[[#]])]),
           (letter,[(U LOWCHAR,[[#]]),(U HIGHCHAR,[[#]])]),
           (reservedid,
            [(U BOOL,[[#]]),(U CALL,[[#]]),(U CONST,[[#]]),(U COPY,[[#]]),
             (U DEBUGIN,[[#]]),(U DEBUGOUT,[[#]]),(U DIVE,[[#]]),
             (U DIVF,[[#]]),(U DIVT,[[#]]),(U DO,[[#]]),(U ELSE,[[#]]),
             (U ENDFUN,[[#]]),(U ENDIF,[[#]]),(U ENDPROC,[[#]]),
             (U ENDPROGRAM,[[#]]),(U ENDWHILE,[[#]]),(U FALSE,[[#]]),
             (U FUN,[[#]]),(U GLOBAL,[[#]]),(U IF,[[#]]),(U IN,[[#]]),
             (U INIT,[[#]]),(U INOUT,[[#]]),(U INT1024,[[#]]),...]),
           (boollit,[(U FALSE,[[#]]),(U TRUE,[[#]])]),
           (intlit,[(U NUMBER,[[#]]),(U TRUE,[[#]])]),
           (literal,[(U TRUE,[[#],[#]]),(U FALSE,[[#]]),(U NUMBER,[[#]])]),
           (ident,
            [(U HIGHCHAR,[[#],[#,#,#],[#,#,#]]),
             (U LOWCHAR,[[#],[#,#,#],[#,#,#]])]),
           (expr,
            [(U LPARENT,[[#,#]]),(U MINUSOPR,[[#,#]]),(U PLUSOPR,[[#,#]]),
             (U NOT,[[#,#]]),(U HIGHCHAR,[[#,#]]),(U LOWCHAR,[[#,#]]),
             (U NUMBER,[[#,#]]),(U TRUE,[[#,#]]),(U FALSE,[[#,#]])]),
           (exprItem,
            [(U AND,[[#,#]]),(U OR,[[#,#]]),($,[[]]),(U COMMA,[[]]),
             (U RPARENT,[[]])]),
           (term1,
            [(U LPARENT,[[#,#,#],[#]]),(U MINUSOPR,[[#,#,#],[#]]),
             (U PLUSOPR,[[#,#,#],[#]]),(U NOT,[[#,#,#],[#]]),
             (U HIGHCHAR,[[#,#,#],[#]]),(U LOWCHAR,[[#,#,#],[#]]),
             (U NUMBER,[[#,#,#],[#]]),(U TRUE,[[#,#,#],[#]]),
             (U FALSE,[[#,#,#],[#]])]),
           (term2,
            [(U LPARENT,[[#,#,#],[#]]),(U MINUSOPR,[[#,#,#],[#]]),
             (U PLUSOPR,[[#,#,#],[#]]),(U NOT,[[#,#,#],[#]]),
             (U HIGHCHAR,[[#,#,#],[#]]),(U LOWCHAR,[[#,#,#],[#]]),
             (U NUMBER,[[#,#,#],[#]]),(U TRUE,[[#,#,#],[#]]),
             (U FALSE,[[#,#,#],[#]])]),
           (term3,
            [(U LPARENT,[[#,#,#],[#]]),(U MINUSOPR,[[#,#,#],[#]]),
             (U PLUSOPR,[[#,#,#],[#]]),(U NOT,[[#,#,#],[#]]),
             (U HIGHCHAR,[[#,#,#],[#]]),(U LOWCHAR,[[#,#,#],[#]]),
             (U NUMBER,[[#,#,#],[#]]),(U TRUE,[[#,#,#],[#]]),
             (U FALSE,[[#,#,#],[#]])]),
           (factor,
            [(U NUMBER,[[#]]),(U TRUE,[[#]]),(U FALSE,[[#]]),
             (U HIGHCHAR,[[#],[#,#],[#,#]]),(U LOWCHAR,[[#],[#,#],[#,#]]),
             (U MINUSOPR,[[#,#]]),(U PLUSOPR,[[#,#]]),(U NOT,[[#,#]]),
             (U LPARENT,[[#,#,#]])]),(exprList,[(U LPARENT,[[#,#,#,#]])]),
           (exprListItem,[(U COMMA,[[#,#]]),(U RPARENT,[[]])]),
           (relopr,
            [(U EQUAL,[[#]]),(U NOTEQUAL,[[#]]),(U GREATER,[[#]]),
             (U GREATEREQUAL,[[#]]),(U LESS,[[#]]),(U LESSEQUAL,[[#]])]),
           (addopr,[(U PLUSOPR,[[#]]),(U MINUSOPR,[[#]])]),
           (monopr,[(U NOT,[[#]]),(U MINUSOPR,[[#]]),(U PLUSOPR,[[#]])])],
       NULLABLE=[(digit,false),(lowercase,false),(uppercase,false),
                 (letter,false),(reservedid,false),(boollit,false),
                 (intlit,false),(literal,false),(ident,false),(expr,false),
                 (exprItem,true),(term1,false),(term2,false),(term3,false),
                 (factor,false),(exprList,false),(exprListItem,true),
                 (relopr,false),(addopr,false),(monopr,false)],S=expr,
       nonterms=[digit,lowercase,uppercase,letter,reservedid,boollit,intlit,
                 literal,ident,expr,exprItem,term1,term2,term3,factor,exprList,
                 exprListItem,relopr,addopr,monopr],
       prods=[(digit,[[T NUMBER]]),(lowercase,[[T LOWCHAR]]),
              (uppercase,[[T HIGHCHAR]]),
              (letter,[[N lowercase],[N uppercase]]),
              (reservedid,
               [[T BOOL],[T CALL],[T CONST],[T COPY],[T DEBUGIN],[T DEBUGOUT],
                [T DIVE],[T DIVF],[T DIVT],[T DO],[T ELSE],[T ENDFUN],
                [T ENDIF],[T ENDPROC],[T ENDPROGRAM],[T ENDWHILE],[T FALSE],
                [T FUN],[T GLOBAL],[T IF],[T IN],[T INIT],[T INOUT],
                [T INT1024],...]),(boollit,[[T FALSE],[T TRUE]]),
              (intlit,[[N digit],[T TRUE]]),(literal,[[N boollit],[N intlit]]),
              (ident,
               [[N letter],[N ident,N digit,N reservedid],
                [N ident,N letter,N reservedid]]),
              (expr,[[N term1,N exprItem]]),
              (exprItem,[[T AND,N term1],[T OR,N term1],[]]),
              (term1,[[N term2,N relopr,N term1],[N term2]]),
              (term2,[[N term3,N addopr,N term2],[N term3]]),
              (term3,[[N factor,N addopr,N term3],[N factor]]),
              (factor,
               [[N literal],[N ident],[N ident,T IDENT],[N ident,N exprList],
                [N monopr,N factor],[T LPARENT,N expr,T RPARENT]]),
              (exprList,[[T LPARENT,N expr,N exprListItem,T RPARENT]]),
              (exprListItem,[[T COMMA,N expr],[]]),
              (relopr,
               [[T EQUAL],[T NOTEQUAL],[T GREATER],[T GREATEREQUAL],[T LESS],
                [T LESSEQUAL]]),(addopr,[[T PLUSOPR],[T MINUSOPR]]),
              (monopr,[[T NOT],[N addopr]])],
       terms=[MINUSOPR,PLUSOPR,LESSEQUAL,LESS,GREATEREQUAL,GREATER,NOTEQUAL,
              EQUAL,COMMA,LPARENT,RPARENT,IDENT,OR,AND,WHILE,VAR,TRUE,THEN,
              SKIP,RETURNS,REF,PROGRAM,PROC,OUT,...]},fn),(fn,fn))
  : (term,?.X1,nonterm) ff'result
val it = () : unit