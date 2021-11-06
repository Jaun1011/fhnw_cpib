datatype term
    = CALL | CONST
    | LOWCHAR
    | HIGHCHAR
    | DEBUGIN
    | DEBUGOUT | DO
    | ELSE | ENDFUN
    | ENDPROGRAM | ENDWHILE 
    | BRACKET_EDGE_R | BRACKET_EDGE_L
    | GLOBAL | IN
    | INOUT | LOCAL
    | MODF | MODT
    | OUT | PROC
    | REF | RETURNS
    | SKIP | THEN
    | TRUE | VAR
    | WHILE | LPARENT
    | BACKSLASH
    | IDENT
    | COMMA
    | RPARENT
    | POINT
    | SEMICOLON
    | ASSIGN
    | OR
    | NEWLINE
    | AND
    | EQUAL
    | NOTEQUAL
    | LESS
    | GREATER
    | LESSEQUAL
    | GREATEREQUAL
    | PLUSOPR
    | MINUSOPR
    | MULTOPR
    | DIVOPR
    | DPOINT
    | FALSE
    | PROGRAM
    | COPY
    | BOOL
    | INT32
    | INT64
    | INT1024
    | FUN
    | ENDPROC
    | NUMBER
    | HIGHCOMMA
    | WHITESPACE
    | IF | ENDIF | INIT | DIVE | MODE | DIVF | DIVT | NOT



val string_of_term = 
    fn CALL => "CALL"
    | WHITESPACE => "WHITESPACE"
    | LOWCHAR => "LOWCHAR"
    | HIGHCHAR => "HIGHCHAR"
    | DEBUGIN => "DEBUGIN"
    | DEBUGOUT => "DEBUGOUT"
    | HIGHCOMMA => "HIGHCOMMA"
    | DPOINT => "DPOINT"
    | DO => "DO"
    | ELSE => "ELSE"
    | ENDFUN => "ENDFUN"
    | BRACKET_EDGE_L => "BRACKET_EDGE_L"
    | BRACKET_EDGE_R => "BRACKET_EDGE_R"
    | ENDPROC => "ENDPROC"
    | FALSE => "FALSE"
    | GLOBAL => "GLOBAL"
    | INIT => "INIT"
    | IDENT => "IDENT"
    | INOUT => "INOUT"
    | INT1024 => "INT1024"
    | INT32 => "INT32"
    | INT64 => "INT64"
    | LOCAL => "LOCAL"
    | MODF => "MODF"
    | MODT => "MODT"
    | BACKSLASH => "BACKSLASH"
    | OUT => "OUT"
    | PROC => "PROC"
    | REF => "REF"
    | RETURNS => "RETURNS"
    | SKIP => "SKIP"
    | THEN => "THEN"
    | TRUE => "TRUE"
    | VAR => "VAR"
    | LPARENT => "LPARENT"
    | COMMA => "COMMA"
    | RPARENT => "RPARENT"
    | POINT => "POINT"
    | SEMICOLON => "SEMICOLON"
    | ASSIGN => "ASSIGN"
    | OR => "OR"
    | AND => "AND"
    | EQUAL => "EQUAL"
    | NOTEQUAL => "NOTEQUAL"
    | LESS => "LESS"
    | GREATER => "GREATER"
    | LESSEQUAL => "LESSEQUAL"
    | GREATEREQUAL => "GREATEREQUAL"
    | PLUSOPR => "PLUSOPR"
    | MINUSOPR => "MINUSOPR"
    | MULTOPR => "MULTOPR"
    | DIVOPR => "DIVOPR"
    | PROGRAM => "PROGRAM"
    | ENDPROGRAM => "ENDPROGRAM"
    | NEWLINE => "NEWLINE"
    | NUMBER => "NUMBER"
    | IN => "IN"
    | COPY => "COPY"
    | CONST => "CONST"
    | BOOL => "BOOL"
    | FUN => "FUN"
    | IF => "IF"
    | ENDIF => "ENDIF"
    | WHILE => "WHILE"
    | ENDWHILE => "ENDWHILE"
    | DIVE => "DIVE"
    | MODE => "MODE"
    | DIVF => "DIVF"
    | DIVT => "DIVT"
    | NOT => "NOT"


datatype nonterm = digit
    | lowercase
    | uppercase
    | letter
    | special
    | space
    | printable
    | linefeed
    | carriet
    | newline
    | whitebase
    | comment
    | whitespace
    | reservedid
    | symbol
    | boollit
    | intlit
    | literal
    | ident
    | lexeme
    | lexemes
    | program
    | flowmode
    | mechmode
    | changemode
    | booltype
    | inttype
    | arraytype
    | typedef
    | primtype
    | progParamList
    | progParamListItem
    | progParam
    | paramList
    | paramListItem
    | param
    | typedIdent
    | decl
    | stoDecl
    | funDecl
    | procDecl
    | globImps
    | globImp
    | cpsDecl
    | cpsDeclItem
    | cpsStoDecl
    | cpsStoDeclItem
    | cmd
    | cpsCmd
    | cpsCmdItem
    | globInits
    | globInitsItem
    | expr
    | exprItem
    | term1
    | term2
    | term3
    | factor
    | exprList
    | relopr
    | addopr
    | multopr
    | divopr
    | monopr
    | chars
    | exprListItem

val string_of_nonterm = 
    fn digit => "digit"
    | lowercase => "lowercase"
    | uppercase => "uppercase"
    | chars => "chars"
    | letter => "letter"
    | special => "special"
    | space => "space"
    | printable => "printable" 
    | cpsDeclItem => "cpsDeclItem" 
    | linefeed => "linefeed"
    | carriet => "carriet"
    | newline => "newline"
    | whitebase => "whitebase"
    | comment => "comment"
    | whitespace => "whitespace"
    | reservedid => "reservedid"
    | symbol => "symbol"
    | boollit => "boollit"
    | intlit => "intlit"
    | literal => "literal"
    | ident => "ident"
    | lexeme => "lexeme"
    | lexemes => "lexemes"
    | program => "program"
    | flowmode => "flowmode"
    | mechmode => "mechmode"
    | changemode => "changemode"
    | booltype => "booltype"
    | inttype => "inttype"
    | arraytype => "arraytype"
    | typedef => "typedef"
    | primtype => "primtype"
    | progParamList => "progParamList"
    | progParamListItem => "progParamListItem"
    | progParam => "progParam"
    | paramList => "paramList"
    | paramListItem => "paramListItem"
    | param => "param"
    | typedIdent => "typedIdent"
    | decl => "decl"
    | stoDecl => "stoDecl"
    | funDecl => "funDecl"
    | procDecl => "procDecl"
    | globImps => "globImps"
    | globImp => "globImp"
    | cpsDecl => "cpsDecl"
    | cpsStoDecl => "cpsStoDecl"
    | cpsStoDeclItem => "cpsStoDeclItem"
    | cmd => "cmd"
    | cpsCmd => "cpsCmd"
    | cpsCmdItem => "cpsCmdItem"
    | globInits => "globInits"
    | globInitsItem => "globInitsItem"
    | expr => "expr"
    | exprItem => "exprItem"
    | term1 => "term1"
    | term2 => "term2"
    | term3 => "term3"
    | factor => "factor"
    | exprList => "exprList"
    | relopr => "relopr"
    | addopr => "addopr"
    | multopr => "multopr"
    | divopr => "divopr"
    | monopr => "monopr"
    | exprListItem => "exprListItem"
    


val string_of_gramsym = (string_of_term, string_of_nonterm)

local
  open FixFoxi.FixFoxiCore
in

val productions = [

    (*# regular definitions*)
    (*## printable characters*)
    (digit,[[T NUMBER]]),
    (lowercase,[[T LOWCHAR]]),
    (uppercase,[[T HIGHCHAR]]),
    (letter, [[N lowercase], [N uppercase]]),
    (chars, [[N letter], [N digit]]),

    (space, [[T WHITESPACE]]),
    (printable, [[N digit], [N letter], [N space]]),


    (*## Comments, Whitespaces*)
    (newline, [[T NEWLINE]]),
    (whitebase, [[N space], [N newline]]),
    (comment, [[T BACKSLASH, T BACKSLASH, N printable]]),
    (whitespace, [[N whitebase], [N comment]]),


    (*## Reserved Identifiers, Symbols*)
    (reservedid,[
            [T BOOL],[T CALL],[T CONST],[T COPY],
            [T DEBUGIN],[T DEBUGOUT],[T DIVE],[T DIVF],
            [T DIVT],[T DO],[T ELSE],[T ENDFUN],[T ENDIF],
            [T ENDPROC],[T ENDPROGRAM],[T ENDWHILE],[T FALSE],
            [T FUN],[T GLOBAL],[T IF],[T IN],[T INIT],[T INOUT],
            [T INT1024],[T INT32],[T INT64],[T LOCAL],[T MODE],
            [T MODF],[T MODT],[T NOT],[T OUT],[T PROC],[T PROGRAM],
            [T REF],[T RETURNS],[T SKIP],[T THEN],[T TRUE],[T VAR],[T WHILE]]),
            
    (special, [
            [T LPARENT],[T COMMA],[T RPARENT],[T POINT],[T SEMICOLON],[T ASSIGN],[T OR],[T AND],
            [T EQUAL],[T NOTEQUAL],[T LESS],[T GREATER],[T LESSEQUAL],[T GREATEREQUAL],
            [T PLUSOPR],[T MINUSOPR],[T MULTOPR],[T DIVOPR]]),

            
    (*## Literals, Identifiers*)
    (boollit, [[T FALSE], [T TRUE]]),
    (intlit, [[N digit], [T TRUE]]),
    (literal, [[N boollit], [N intlit]]),
    (ident, [[N letter], 
             [N ident, N digit, N reservedid],
             [N ident, N letter, N reservedid]
    ]),


    (*# Top Level*)
    (*## programs*)
    (program, [[T PROGRAM, N ident, 
                T DO, N cpsCmd, T ENDPROGRAM],
               [T PROGRAM, N ident, T GLOBAL, N cpsDecl, 
                T DO, N cpsCmd, T ENDPROGRAM]]),

    (*## modes*)
    (flowmode, [[T IN], [T OUT], [T INOUT]]),
    (mechmode, [[T COPY], [T REF]]),
    (changemode, [[T CONST], [T VAR]]),


    (*## Types*)
    (booltype,[[T BOOL]]),
    (inttype,[[T INT32],[T INT64],[T INT1024]]),
    (primtype,[[N booltype],[N inttype]]),

    (arraytype, [[N primtype, T BRACKET_EDGE_L, N intlit, T BRACKET_EDGE_R]]),
    (typedef, [[N primtype],[N arraytype]]),

    (*## Parameter List*)
    (progParamList, [[T LPARENT, N progParam, T RPARENT, N progParamListItem]]),
    (progParamListItem, [[T COMMA, N progParam],[]]),
    (progParam, [[N flowmode],[N changemode], [N typedIdent]]),
    (paramList, [[T LPARENT, N progParam, N paramListItem, T RPARENT]]),
    (paramListItem, [[T COMMA, N param], []]),
    (param, [[N flowmode],[N mechmode], [N changemode], [N typedIdent]]),
    (typedIdent, [[N ident, T DPOINT, N typedef]]),

    (*## Declarations*)
    (decl, [[N stoDecl],[N funDecl], [N procDecl]]),
    (stoDecl, [[N typedIdent], [N changemode, N typedIdent]]),
    (funDecl, [[T FUN, N ident, N paramList, T RETURNS, N stoDecl],
               [T GLOBAL, N globImps , T LOCAL, N cpsStoDecl],
               [T GLOBAL, N globImps],
               [T LOCAL, N cpsStoDecl],
               [T DO, N cpsCmd, T ENDPROC]]),

    (procDecl, [[T PROC, N ident, N paramList, T GLOBAL, N globImps, T LOCAL, N cpsStoDecl, T DO, N cpsCmd, T ENDPROC],
                [T PROC, N ident, N paramList, T GLOBAL, N globImps, T DO, N cpsCmd, T ENDPROC],
                [T PROC, N ident, N paramList, T LOCAL, N cpsStoDecl, T DO, N cpsCmd, T ENDPROC]]),
    (globImps, [[N globImp], [N globImp, T COMMA, N globImps]]),
    (globImp, [[N flowmode, N changemode, N ident],
               [N changemode, N ident],
               [N flowmode, N ident],
               []]),

    (cpsDecl, [[N decl, N cpsDeclItem]]),
    (cpsDeclItem, [[T COMMA, N decl], []]),

    (cpsStoDecl, [[N stoDecl, N cpsStoDeclItem]]),
    (cpsStoDeclItem, [[T COMMA, N stoDecl], []]),

    (*## Commands*)
    (cmd, [[T SKIP], 
                      [N expr, T ASSIGN, N expr],
                      [T IF, N expr, T THEN, N cpsCmd ,T ENDIF],
                      [T IF, N expr, T THEN, N cpsCmd ,T ELSE, N cpsCmd, T ENDIF],
                      [T WHILE, N expr, T DO, N cpsCmd, T ENDWHILE],
                      [T CALL, N ident, N exprList],
                      [T CALL, N ident, N exprList, N globInits],
                      [T DEBUGIN, N expr],
                      [T DEBUGOUT, N expr]]),

    (cpsCmd, [[N cmd, N cpsCmdItem]]),
    (cpsCmdItem, [[T SEMICOLON, N cmd],[]]),
    (globInits, [[T INIT, N ident, N globInitsItem]]),
    (globInitsItem, [[T COMMA, N ident]]),
    
    (*## Expression*)
    (expr, [[N term1, N exprItem]]),
    (exprItem, [[T AND, N term1], [T OR, N term1], []]),
    (term1, [[N term2, N relopr, N term1], [N term2]]),
    (term2, [[N term3, N addopr, N term2], [N term3]]),
    (term3, [[N factor, N addopr, N term3], [N factor]]),
    (factor, [[N literal],[N ident],
              [N ident,T IDENT],
              [N ident, N exprList],
              [N monopr, N factor],
              [T LPARENT, N expr, T RPARENT]]),
    (exprList, [[T LPARENT, N expr,N exprListItem, T RPARENT]]),
    (exprListItem, [[T COMMA, N expr], []]),


    (*## operators*)
    (relopr, [[T EQUAL], [T NOTEQUAL], [T GREATER], [T GREATEREQUAL], [T LESS], [T LESSEQUAL]]),
    (addopr, [[T PLUSOPR], [T MINUSOPR]]),
    (multopr, [[T MULTOPR], [N divopr]]),
    (divopr, [[T DIVE], [T MODE], [T DIVF], [T MODF], [T DIVT], [T MODT]]),
    (monopr, [[T NOT], [N addopr]])
]

val S = expr

val result = fix_foxi productions S string_of_gramsym

end (* local *)
