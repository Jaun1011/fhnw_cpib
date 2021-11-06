Windows PowerShell
Copyright (C) Microsoft Corporation. Alle Rechte vorbehalten.

Lernen Sie das neue plattformübergreifende PowerShell kennen – https://aka.ms/pscore6

PS C:\Users\jan\Downloads\FixFoxi_V5> cd .\src\
PS C:\Users\jan\Downloads\FixFoxi_V5\src> sml
Standard ML of New Jersey, Version 110.0.7, September 28, 2000 [CM&CMB]
-  use "use.sml";
[opening use.sml] 
val it = () : unit
printLength = 24  
printDepth  = 10  
stringDepth = 140 
val it = () : unit
[opening BASIC.sig]
signature BASIC =
  sig
    exception Lookup
    exception Zip
    val I : 'a -> 'a
    val K : 'b -> 'a -> 'b
    val non : ('a -> bool) -> 'a -> bool
    val pair : 'a -> 'b -> 'a * 'b      
    val fst : 'b * 'a -> 'b
    val snd : 'a * 'b -> 'b
    val swap : 'b * 'a -> 'a * 'b
    val member : ''a * ''a list -> bool
    val lookup : (''a * 'b) list -> ''a -> 'b option
    val lookupE : (''a * 'b) list -> ''a -> 'b
    val zipE : 'a list * 'b list -> ('a * 'b) list
  end
[opening SET.sig]
signature SET =
  sig
    type 'a t
    exception E
    val setof : ''a list -> ''a t
    val listof : 'a t -> 'a list
    val insert : ''a * ''a t -> ''a t
    val is_empty : 'a t -> bool
    val choose : 'a t -> 'a * 'a t
    val elem : ''a * ''a t -> bool
    val empty : 'a t
    val setmap : ('a -> 'b) -> 'a t -> 'b t
    val union : ''a t * ''a t -> ''a t
    val inter : ''a t * ''a t -> ''a t
    val diff : ''a t * ''a t -> ''a t
    val subset : ''a t * ''a t -> bool
    val seteq : ''a t * ''a t -> bool
    val Union : ''a t list -> ''a t
  end
[opening FIX_FOXI_CORE.sig]
signature FIX_FOXI_CORE =
  sig
    datatype ('a,'b) grammar_symbol = N of 'b | T of 'a
    type ('a,'b) productions = ('b * ('a,'b) grammar_symbol list list) list
    datatype 'a terminal_or_end = $ | U of 'a
    type ('a,'b) parse_tab_mult_entries =
      ('b * ('a terminal_or_end * ('a,'b) grammar_symbol list list) list) list
    type 'a warnings1 =
      {empty_RHSs:'a list, multiples_LHS:'a list, unproductives:'a list}
    type ('a,'b) warnings2 =
      {grammar_not_LL1:('a,'b) parse_tab_mult_entries, unreachables:'b list}
    type ('a,'b) analysis =
      {FIRST:('b * 'a list) list, FOLLOW:('b * 'a terminal_or_end list) list,
       MM:('a,'b) parse_tab_mult_entries, NULLABLE:('b * bool) list, S:'b,
       nonterms:'b list, prods:('a,'b) productions, terms:'a list}
    datatype ('a,'b) parse_tree = PN of 'b * ('a,'b) parse_tree list | TL of 'a
    type term_leaf_location = int * (int * int)
    type prod_node_location = ((int * int) * (int * int)) option
    type ('a,'b) parser =
      string * ('a * term_leaf_location) list
      -> ('a * term_leaf_location,'b * prod_node_location) parse_tree
    type ('a,'b) string_of_gramsym = ('a -> string) * ('b -> string)
    type ('a,'b,'c) fix_foxi'result =
      'c warnings1
      * (('a,'c) warnings2 * ('a,'c) analysis * ('a * 'b,'c) parser) option
      * ('a,'c) string_of_gramsym
    val fix_foxi'core : (''a,''c) productions
                        -> ''c
                           -> (''a,''c) string_of_gramsym
                              -> (''a,'b,''c) fix_foxi'result
    exception SyntaxError'_terminal_terminal_mismatch
    exception SyntaxError'_nonterminal_terminal_mismatch
  end
[opening FIX_FOXI.sig]
signature FIX_FOXI =
  sig
    structure FixFoxiCore :
      sig
        datatype ('a,'b) grammar_symbol = N of 'b | T of 'a
        type ('a,'b) productions = ('b * ('a,'b) grammar_symbol list list) list
        datatype 'a terminal_or_end = $ | U of 'a
        type ('a,'b) parse_tab_mult_entries =
          ('b * ('a terminal_or_end * ('a,'b) grammar_symbol list list) list)
            list
        type 'a warnings1 =
          {empty_RHSs:'a list, multiples_LHS:'a list, unproductives:'a list}
        type ('a,'b) warnings2 =
          {grammar_not_LL1:('a,'b) parse_tab_mult_entries,
           unreachables:'b list}
        type ('a,'b) analysis =
          {FIRST:('b * 'a list) list,
           FOLLOW:('b * 'a terminal_or_end list) list,
           MM:('a,'b) parse_tab_mult_entries, NULLABLE:('b * bool) list, S:'b,
           nonterms:'b list, prods:('a,'b) productions, terms:'a list}
        datatype ('a,'b) parse_tree
          = PN of 'b * ('a,'b) parse_tree list | TL of 'a
        type term_leaf_location = int * (int * int)
        type prod_node_location = ((int * int) * (int * int)) option
        type ('a,'b) parser =
          string * ('a * term_leaf_location) list
          -> ('a * term_leaf_location,'b * prod_node_location) parse_tree
        type ('a,'b) string_of_gramsym = ('a -> string) * ('b -> string)
        type ('a,'b,'c) fix_foxi'result =
          'c warnings1
          * (('a,'c) warnings2 * ('a,'c) analysis * ('a * 'b,'c) parser) option
          * ('a,'c) string_of_gramsym
        val fix_foxi'core : (''a,''c) productions
                            -> ''c
                               -> (''a,''c) string_of_gramsym
                                  -> (''a,'b,''c) fix_foxi'result
        exception SyntaxError'_terminal_terminal_mismatch
        exception SyntaxError'_nonterminal_terminal_mismatch
      end
    exception GrammarError'_empty_language
    exception GrammarError'_unexpected_warnings
    type ('a,'b,'c) ff'result = ('a,'b,'c) FixFoxiCore.fix_foxi'result
    val dispDiagnosis : ('a,'b,'c) ff'result -> unit
    val dispTerms : ('a,'b,'c) ff'result -> unit
    val dispNonterms : ('a,'b,'c) ff'result -> unit
    val dispProds : ('a,'b,'c) ff'result -> unit
    val dispS : ('a,'b,'c) ff'result -> unit
    val dispNULLABLE : ('a,'b,'c) ff'result -> unit
    val dispFIRST : ('a,'b,'c) ff'result -> unit
    val dispFOLLOW : ('a,'b,'c) ff'result -> unit
    val dispMM : ('a,'b,'c) ff'result -> unit
    val ? : unit -> unit
    val extractParser : (''a,'b,''c) ff'result * ''c FixFoxiCore.warnings1
                        * (''a,''c) FixFoxiCore.warnings2
                        -> (''a * 'b,''c) FixFoxiCore.parser
    val fix_foxi : (''a,''c) FixFoxiCore.productions
                   -> ''c
                      -> (''a,''c) FixFoxiCore.string_of_gramsym
                         -> (''a,'b,''c) ff'result
  end
[opening Basic.fun]
functor BasicFUN : <sig>
[opening Set.fun]
functor SetFUN : <sig>
[opening FixFoxiCore.fun]
functor FixFoxiCoreFUN : <sig>
[opening FixFoxi.fun]
functor FixFoxiFUN : <sig>
val it = () : unit
structure Basic : BASIC
structure Set : SET
structure FixFoxiCore : FIX_FOXI_CORE
structure FixFoxi : FIX_FOXI
opening FixFoxi
  structure FixFoxiCore :
    sig
      datatype ('a,'b) grammar_symbol = N of 'b | T of 'a
      type ('a,'b) productions = ('b * ('a,'b) grammar_symbol list list) list
      datatype 'a terminal_or_end = $ | U of 'a
      type ('a,'b) parse_tab_mult_entries =
        ('b * ('a terminal_or_end * ('a,'b) grammar_symbol list list) list)
          list
      type 'a warnings1 =
        {empty_RHSs:'a list, multiples_LHS:'a list, unproductives:'a list}
      type ('a,'b) warnings2 =
        {grammar_not_LL1:('a,'b) parse_tab_mult_entries, unreachables:'b list}
      type ('a,'b) analysis =
        {FIRST:('b * 'a list) list, FOLLOW:('b * 'a terminal_or_end list) list,
         MM:('a,'b) parse_tab_mult_entries, NULLABLE:('b * bool) list, S:'b,
         nonterms:'b list, prods:('a,'b) productions, terms:'a list}
      datatype ('a,'b) parse_tree
        = PN of 'b * ('a,'b) parse_tree list | TL of 'a
      type term_leaf_location = int * (int * int)
      type prod_node_location = ((int * int) * (int * int)) option
      type ('a,'b) parser =
        string * ('a * term_leaf_location) list
        -> ('a * term_leaf_location,'b * prod_node_location) parse_tree
      type ('a,'b) string_of_gramsym = ('a -> string) * ('b -> string)
      type ('a,'b,'c) fix_foxi'result =
        'c warnings1
        * (('a,'c) warnings2 * ('a,'c) analysis * ('a * 'b,'c) parser) option
        * ('a,'c) string_of_gramsym
      val fix_foxi'core : (''a,''c) productions
                          -> ''c
                             -> (''a,''c) string_of_gramsym
                                -> (''a,'b,''c) fix_foxi'result
      exception SyntaxError'_terminal_terminal_mismatch
      exception SyntaxError'_nonterminal_terminal_mismatch
    end
  exception GrammarError'_empty_language
  exception GrammarError'_unexpected_warnings
  type ('a,'b,'c) ff'result = ('a,'b,'c) ?.FixFoxiCore.fix_foxi'result
  val dispDiagnosis : ('a,'b,'c) ff'result -> unit
  val dispTerms : ('a,'b,'c) ff'result -> unit
  val dispNonterms : ('a,'b,'c) ff'result -> unit
  val dispProds : ('a,'b,'c) ff'result -> unit
  val dispS : ('a,'b,'c) ff'result -> unit
  val dispNULLABLE : ('a,'b,'c) ff'result -> unit
  val dispFIRST : ('a,'b,'c) ff'result -> unit
  val dispFOLLOW : ('a,'b,'c) ff'result -> unit
  val dispMM : ('a,'b,'c) ff'result -> unit
  val ? : unit -> unit
  val extractParser : (''a,'b,''c) ff'result * ''c FixFoxiCore.warnings1
                      * (''a,''c) FixFoxiCore.warnings2
                      -> (''a * 'b,''c) FixFoxiCore.parser
  val fix_foxi : (''a,''c) FixFoxiCore.productions
                 -> ''c
                    -> (''a,''c) FixFoxiCore.string_of_gramsym
                       -> (''a,'b,''c) ff'result
val it = () : unit
- OS.FileSys.chDir "..\\Grammars";
val it = () : unit
- use "grammar.sml";
[opening grammar.sml]
grammar.sml:171.1-280.2 Error: operator and operand don't agree [tycon mismatch]
  operator domain: (nonterm * (term,term) grammar_symbol list list)
                   * (nonterm * (term,term) grammar_symbol list list) list
  operand:         (nonterm * (term,term) grammar_symbol list list)
                   * (nonterm * (term,nonterm) grammar_symbol list list) list
  in expression:
    (underlinemechmodeOptional,(N MECHMODE :: nil) :: nil :: nil) ::
      (underlinecommaMany,
       (T COMMA :: N param :: N underlinecommaMany :: nil) :: nil :: nil) ::
        (underlineprogParamMany,
         (T COMMA :: N progParam :: N underlineprogParamMany :: nil) ::
           nil :: nil) :: nil
grammar.sml:171.1-280.2 Error: operator and operand don't agree [tycon mismatch]
  operator domain: (nonterm * (nonterm,nonterm) grammar_symbol list list)
                   * (nonterm * (nonterm,nonterm) grammar_symbol list list)
                       list
  operand:         (nonterm * (nonterm,nonterm) grammar_symbol list list)
                   * (nonterm * (term,nonterm) grammar_symbol list list) list
  in expression:
    (progParam,
     (N underlineflowModeOptional ::
       N underlinechangeModeOptional :: T typedIdent :: nil) :: nil) ::
      (paramList,
       (T LPAREN :: N underlineparamOptional :: T RPAREN :: nil) :: nil) ::
        (param,
         (N underlineflowModeOptional ::
           N underlinemechmodeOptional ::
             N underlinechangeModeOptional :: N <exp> :: nil) :: nil) ::
          (typedIdent,
           (T IDENT :: T COLON :: T <exp> :: nil) ::
             (T IDENT :: T <exp> :: <exp> :: <exp>) ::
               (T <exp> :: <exp> :: <exp>) :: nil) ::
            (underlineprogParamOptional,
             (N progParam :: N <exp> :: nil) :: nil :: nil) ::
              (underlinechangeModeOptional,(T <exp> :: nil) :: nil :: nil) ::
                (underlineparamOptional,(<exp> :: <exp>) :: <exp> :: <exp>) ::
                  (underlinemechmodeOptional,<exp> :: <exp>) ::
                    (<exp>,<exp>) :: <exp> :: <exp>
grammar.sml:284.1-284.70 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
- use "grammar.sml";
[opening grammar.sml]
grammar.sml:332.1-332.54 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)      
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
val string_of_term = fn : term -> string
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
val string_of_nonterm = fn : nonterm -> string
val string_of_gramsym = (fn,fn) : (term -> string) * (nonterm -> string)
val productions =
  [(opt_GLOBALcpsDecl,[[T GLOBAL,N cpsDecl],[]]),
   (program,
    [[T PROGRAM,T IDENT,N progParamList,N opt_GLOBALcpsDecl,T DO,N cpsCmd,
      T ENDPROGRAM]]),(decl,[[N stoDecl],[N funDecl],[N procDecl]]),
   (opt_CHANGEMODE,[[T CHANGEMODE],[]]),
   (stoDecl,[[N opt_CHANGEMODE,N typedIdent]]),
   (opt_GLOBALglobImps,[[T GLOBAL,N globImps],[]]),
   (opt_LOCALcpsStoDecl,[[T LOCAL,N cpsStoDecl],[]]),
   (funDecl,
    [[T FUN,T IDENT,N paramList,T RETURNS,N stoDecl,N opt_GLOBALglobImps,
      N opt_LOCALcpsStoDecl,T DO,N cpsCmd,T ENDFUN]]),
   (procDecl,
    [[T PROC,T IDENT,N paramList,N opt_GLOBALglobImps,N opt_LOCALcpsStoDecl,
      T DO,N cpsCmd,T ENDPROC]]),
   (rep_COMMAglobImp,[[T COMMA,N globImp,N rep_COMMAglobImp],[]]),
   (globImps,[[N globImp,N rep_COMMAglobImp]]),
   (opt_FLOWMODE,[[T FLOWMODE],[]]),
   (globImp,[[N opt_FLOWMODE,N opt_CHANGEMODE,T IDENT]]),
   (rep_SEMICOLONdecl,[[T SEMICOLON,N decl,N rep_SEMICOLONdecl],[]]),
   (cpsDecl,[[N decl,N rep_SEMICOLONdecl]]),
   (rep_SEMICOLONstoDecl,[[T SEMICOLON,N stoDecl,N rep_SEMICOLONstoDecl],[]]),
   (cpsStoDecl,[[N stoDecl,N rep_SEMICOLONstoDecl]]),
   (rep_COMMAprogParam,[[T COMMA,N progParam,N rep_COMMAprogParam],[]]),
   (opt_progParamrep_COMMAprogParam,[[N progParam,N rep_COMMAprogParam],[]]),
   (progParamList,[[T LPAREN,N opt_progParamrep_COMMAprogParam,T RPAREN]]),
   (progParam,[[N opt_FLOWMODE,N opt_CHANGEMODE,N typedIdent]]),
   (rep_COMMAparam,[[T COMMA,N param,N rep_COMMAparam],[]]),
   (opt_paramrep_COMMAparam,[[N param,N rep_COMMAparam],[]]),
   (paramList,[[T LPAREN,N opt_paramrep_COMMAparam,T RPAREN]]),...]
  : (nonterm * (term,nonterm) FixFoxiCore.grammar_symbol list list) list
val S = program : nonterm
val result =
  ({empty_RHSs=[],multiples_LHS=[],unproductives=[]},
   SOME
     ({grammar_not_LL1=[],unreachables=[]},
      {FIRST=[(opt_GLOBALcpsDecl,[GLOBAL]),(program,[PROGRAM]),
              (decl,[PROC,FUN,IDENT,CHANGEMODE]),(opt_CHANGEMODE,[CHANGEMODE]),
              (stoDecl,[IDENT,CHANGEMODE]),(opt_GLOBALglobImps,[GLOBAL]),
              (opt_LOCALcpsStoDecl,[LOCAL]),(funDecl,[FUN]),(procDecl,[PROC]),
              (rep_COMMAglobImp,[COMMA]),
              (globImps,[IDENT,CHANGEMODE,FLOWMODE]),(opt_FLOWMODE,[FLOWMODE]),
              (globImp,[IDENT,CHANGEMODE,FLOWMODE]),
              (rep_SEMICOLONdecl,[SEMICOLON]),
              (cpsDecl,[PROC,FUN,IDENT,CHANGEMODE]),
              (rep_SEMICOLONstoDecl,[SEMICOLON]),
              (cpsStoDecl,[IDENT,CHANGEMODE]),(rep_COMMAprogParam,[COMMA]),
              (opt_progParamrep_COMMAprogParam,[IDENT,CHANGEMODE,FLOWMODE]),
              (progParamList,[LPAREN]),(progParam,[IDENT,CHANGEMODE,FLOWMODE]),
              (rep_COMMAparam,[COMMA]),
              (opt_paramrep_COMMAparam,[IDENT,CHANGEMODE,MECHMODE,FLOWMODE]),
              (paramList,[LPAREN]),...],
       FOLLOW=[(opt_GLOBALcpsDecl,[U DO]),(program,[$]),
               (decl,[U DO,U SEMICOLON]),(opt_CHANGEMODE,[U IDENT]),
               (stoDecl,[U LOCAL,U GLOBAL,U DO,U SEMICOLON]),
               (opt_GLOBALglobImps,[U DO,U LOCAL]),
               (opt_LOCALcpsStoDecl,[U DO]),(funDecl,[U DO,U SEMICOLON]),
               (procDecl,[U DO,U SEMICOLON]),(rep_COMMAglobImp,[U DO,U LOCAL]),
               (globImps,[U DO,U LOCAL]),
               (opt_FLOWMODE,[U MECHMODE,U IDENT,U CHANGEMODE]),
               (globImp,[U DO,U LOCAL,U COMMA]),(rep_SEMICOLONdecl,[U DO]),
               (cpsDecl,[U DO]),(rep_SEMICOLONstoDecl,[U DO]),
               (cpsStoDecl,[U DO]),(rep_COMMAprogParam,[U RPAREN]),
               (opt_progParamrep_COMMAprogParam,[U RPAREN]),
               (progParamList,[U DO,U GLOBAL]),(progParam,[U RPAREN,U COMMA]),
               (rep_COMMAparam,[U RPAREN]),
               (opt_paramrep_COMMAparam,[U RPAREN]),
               (paramList,[U DO,U LOCAL,U GLOBAL,U RETURNS]),...],
       MM=[(opt_GLOBALcpsDecl,[(U GLOBAL,[[#,#]]),(U DO,[[]])]),
           (program,[(U PROGRAM,[[#,#,#,#,#,#,#]])]),
           (decl,
            [(U IDENT,[[#]]),(U CHANGEMODE,[[#]]),(U FUN,[[#]]),
             (U PROC,[[#]])]),
           (opt_CHANGEMODE,[(U CHANGEMODE,[[#]]),(U IDENT,[[]])]),
           (stoDecl,[(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]])]),
           (opt_GLOBALglobImps,
            [(U GLOBAL,[[#,#]]),(U DO,[[]]),(U LOCAL,[[]])]),
           (opt_LOCALcpsStoDecl,[(U LOCAL,[[#,#]]),(U DO,[[]])]),
           (funDecl,[(U FUN,[[#,#,#,#,#,#,#,#,#,#]])]),
           (procDecl,[(U PROC,[[#,#,#,#,#,#,#,#]])]),
           (rep_COMMAglobImp,[(U COMMA,[[#,#,#]]),(U DO,[[]]),(U LOCAL,[[]])]),
           (globImps,
            [(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]]),(U FLOWMODE,[[#,#]])]),
           (opt_FLOWMODE,
            [(U FLOWMODE,[[#]]),(U MECHMODE,[[]]),(U IDENT,[[]]),
             (U CHANGEMODE,[[]])]),
           (globImp,
            [(U IDENT,[[#,#,#]]),(U CHANGEMODE,[[#,#,#]]),
             (U FLOWMODE,[[#,#,#]])]),
           (rep_SEMICOLONdecl,[(U SEMICOLON,[[#,#,#]]),(U DO,[[]])]),
           (cpsDecl,
            [(U PROC,[[#,#]]),(U FUN,[[#,#]]),(U IDENT,[[#,#]]),
             (U CHANGEMODE,[[#,#]])]),
           (rep_SEMICOLONstoDecl,[(U SEMICOLON,[[#,#,#]]),(U DO,[[]])]),
           (cpsStoDecl,[(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]])]),
           (rep_COMMAprogParam,[(U COMMA,[[#,#,#]]),(U RPAREN,[[]])]),
           (opt_progParamrep_COMMAprogParam,
            [(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]]),(U FLOWMODE,[[#,#]]),
             (U RPAREN,[[]])]),(progParamList,[(U LPAREN,[[#,#,#]])]),
           (progParam,
            [(U IDENT,[[#,#,#]]),(U CHANGEMODE,[[#,#,#]]),
             (U FLOWMODE,[[#,#,#]])]),
           (rep_COMMAparam,[(U COMMA,[[#,#,#]]),(U RPAREN,[[]])]),
           (opt_paramrep_COMMAparam,
            [(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]]),(U MECHMODE,[[#,#]]),
             (U FLOWMODE,[[#,#]]),(U RPAREN,[[]])]),
           (paramList,[(U LPAREN,[[#,#,#]])]),...],
       NULLABLE=[(opt_GLOBALcpsDecl,true),(program,false),(decl,false),
                 (opt_CHANGEMODE,true),(stoDecl,false),
                 (opt_GLOBALglobImps,true),(opt_LOCALcpsStoDecl,true),
                 (funDecl,false),(procDecl,false),(rep_COMMAglobImp,true),
                 (globImps,false),(opt_FLOWMODE,true),(globImp,false),
                 (rep_SEMICOLONdecl,true),(cpsDecl,false),
                 (rep_SEMICOLONstoDecl,true),(cpsStoDecl,false),
                 (rep_COMMAprogParam,true),
                 (opt_progParamrep_COMMAprogParam,true),(progParamList,false),
                 (progParam,false),(rep_COMMAparam,true),
                 (opt_paramrep_COMMAparam,true),(paramList,false),...],
       S=program,
       nonterms=[opt_GLOBALcpsDecl,program,decl,opt_CHANGEMODE,stoDecl,
                 opt_GLOBALglobImps,opt_LOCALcpsStoDecl,funDecl,procDecl,
                 rep_COMMAglobImp,globImps,opt_FLOWMODE,globImp,
                 rep_SEMICOLONdecl,cpsDecl,rep_SEMICOLONstoDecl,cpsStoDecl,
                 rep_COMMAprogParam,opt_progParamrep_COMMAprogParam,
                 progParamList,progParam,rep_COMMAparam,
                 opt_paramrep_COMMAparam,paramList,...],
       prods=[(opt_GLOBALcpsDecl,[[T GLOBAL,N cpsDecl],[]]),
              (program,
               [[T PROGRAM,T IDENT,N progParamList,N opt_GLOBALcpsDecl,T DO,
                 N cpsCmd,T ENDPROGRAM]]),
              (decl,[[N stoDecl],[N funDecl],[N procDecl]]),
              (opt_CHANGEMODE,[[T CHANGEMODE],[]]),
              (stoDecl,[[N opt_CHANGEMODE,N typedIdent]]),
              (opt_GLOBALglobImps,[[T GLOBAL,N globImps],[]]),
              (opt_LOCALcpsStoDecl,[[T LOCAL,N cpsStoDecl],[]]),
              (funDecl,
               [[T FUN,T IDENT,N paramList,T RETURNS,N stoDecl,
                 N opt_GLOBALglobImps,N opt_LOCALcpsStoDecl,T DO,N cpsCmd,
                 T ENDFUN]]),
              (procDecl,
               [[T PROC,T IDENT,N paramList,N opt_GLOBALglobImps,
                 N opt_LOCALcpsStoDecl,T DO,N cpsCmd,T ENDPROC]]),
              (rep_COMMAglobImp,[[T COMMA,N globImp,N rep_COMMAglobImp],[]]),
              (globImps,[[N globImp,N rep_COMMAglobImp]]),
              (opt_FLOWMODE,[[T FLOWMODE],[]]),
              (globImp,[[N opt_FLOWMODE,N opt_CHANGEMODE,T IDENT]]),
              (rep_SEMICOLONdecl,
               [[T SEMICOLON,N decl,N rep_SEMICOLONdecl],[]]),
              (cpsDecl,[[N decl,N rep_SEMICOLONdecl]]),
              (rep_SEMICOLONstoDecl,
               [[T SEMICOLON,N stoDecl,N rep_SEMICOLONstoDecl],[]]),
              (cpsStoDecl,[[N stoDecl,N rep_SEMICOLONstoDecl]]),
              (rep_COMMAprogParam,
               [[T COMMA,N progParam,N rep_COMMAprogParam],[]]),
              (opt_progParamrep_COMMAprogParam,
               [[N progParam,N rep_COMMAprogParam],[]]),
              (progParamList,
               [[T LPAREN,N opt_progParamrep_COMMAprogParam,T RPAREN]]),
              (progParam,[[N opt_FLOWMODE,N opt_CHANGEMODE,N typedIdent]]),
              (rep_COMMAparam,[[T COMMA,N param,N rep_COMMAparam],[]]),
              (opt_paramrep_COMMAparam,[[N param,N rep_COMMAparam],[]]),
              (paramList,[[T LPAREN,N opt_paramrep_COMMAparam,T RPAREN]]),...],
       terms=[NOT,LITERAL,MULTOPR,ADDOPR,RELOPR,BOOLOPR,INIT,DEBUGOUT,DEBUGIN,
              CALL,WHILE,ENDWHILE,IF,THEN,ELSE,ENDIF,BECOMES,SKIP,COLON,
              ATOMTYPE,MECHMODE,LPAREN,RPAREN,SEMICOLON,...]},fn),(fn,fn))
  : (term,?.X1,nonterm) ff'result
val it = () : unit
- use "grammar.sml";
[opening grammar.sml]
grammar.sml:325.1-325.54 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
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
val string_of_term = fn : term -> string
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
val string_of_nonterm = fn : nonterm -> string
val string_of_gramsym = (fn,fn) : (term -> string) * (nonterm -> string)
val productions =
  [(opt_GLOBALcpsDecl,[[T GLOBAL,N cpsDecl],[]]),
   (program,
    [[T PROGRAM,T IDENT,N progParamList,N opt_GLOBALcpsDecl,T DO,N cpsCmd,
      T ENDPROGRAM]]),(decl,[[N stoDecl],[N funDecl],[N procDecl]]),
   (opt_CHANGEMODE,[[T CHANGEMODE],[]]),
   (stoDecl,[[N opt_CHANGEMODE,N typedIdent]]),
   (opt_GLOBALglobImps,[[T GLOBAL,N globImps],[]]),
   (opt_LOCALcpsStoDecl,[[T LOCAL,N cpsStoDecl],[]]),
   (funDecl,
    [[T FUN,T IDENT,N paramList,T RETURNS,N stoDecl,N opt_GLOBALglobImps,
      N opt_LOCALcpsStoDecl,T DO,N cpsCmd,T ENDFUN]]),
   (procDecl,
    [[T PROC,T IDENT,N paramList,N opt_GLOBALglobImps,N opt_LOCALcpsStoDecl,
      T DO,N cpsCmd,T ENDPROC]]),
   (rep_COMMAglobImp,[[T COMMA,N globImp,N rep_COMMAglobImp],[]]),
   (globImps,[[N globImp,N rep_COMMAglobImp]]),
   (opt_FLOWMODE,[[T FLOWMODE],[]]),
   (globImp,[[N opt_FLOWMODE,N opt_CHANGEMODE,T IDENT]]),
   (rep_SEMICOLONdecl,[[T SEMICOLON,N decl,N rep_SEMICOLONdecl],[]]),
   (cpsDecl,[[N decl,N rep_SEMICOLONdecl]]),
   (rep_SEMICOLONstoDecl,[[T SEMICOLON,N stoDecl,N rep_SEMICOLONstoDecl],[]]),
   (cpsStoDecl,[[N stoDecl,N rep_SEMICOLONstoDecl]]),
   (rep_COMMAprogParam,[[T COMMA,N progParam,N rep_COMMAprogParam],[]]),
   (opt_progParamrep_COMMAprogParam,[[N progParam,N rep_COMMAprogParam],[]]),
   (progParamList,[[T LPAREN,N opt_progParamrep_COMMAprogParam,T RPAREN]]),
   (progParam,[[N opt_FLOWMODE,N opt_CHANGEMODE,N typedIdent]]),
   (rep_COMMAparam,[[T COMMA,N param,N rep_COMMAparam],[]]),
   (opt_paramrep_COMMAparam,[[N param,N rep_COMMAparam],[]]),
   (paramList,[[T LPAREN,N opt_paramrep_COMMAparam,T RPAREN]]),...]
  : (nonterm * (term,nonterm) FixFoxiCore.grammar_symbol list list) list
val S = program : nonterm
val result =
  ({empty_RHSs=[],multiples_LHS=[],unproductives=[]},
   SOME
     ({grammar_not_LL1=[],unreachables=[]},
      {FIRST=[(opt_GLOBALcpsDecl,[GLOBAL]),(program,[PROGRAM]),
              (decl,[PROC,FUN,IDENT,CHANGEMODE]),(opt_CHANGEMODE,[CHANGEMODE]),
              (stoDecl,[IDENT,CHANGEMODE]),(opt_GLOBALglobImps,[GLOBAL]),
              (opt_LOCALcpsStoDecl,[LOCAL]),(funDecl,[FUN]),(procDecl,[PROC]),
              (rep_COMMAglobImp,[COMMA]),
              (globImps,[IDENT,CHANGEMODE,FLOWMODE]),(opt_FLOWMODE,[FLOWMODE]),
              (globImp,[IDENT,CHANGEMODE,FLOWMODE]),
              (rep_SEMICOLONdecl,[SEMICOLON]),
              (cpsDecl,[PROC,FUN,IDENT,CHANGEMODE]),
              (rep_SEMICOLONstoDecl,[SEMICOLON]),
              (cpsStoDecl,[IDENT,CHANGEMODE]),(rep_COMMAprogParam,[COMMA]),
              (opt_progParamrep_COMMAprogParam,[IDENT,CHANGEMODE,FLOWMODE]),
              (progParamList,[LPAREN]),(progParam,[IDENT,CHANGEMODE,FLOWMODE]),
              (rep_COMMAparam,[COMMA]),
              (opt_paramrep_COMMAparam,[IDENT,CHANGEMODE,MECHMODE,FLOWMODE]),
              (paramList,[LPAREN]),...],
       FOLLOW=[(opt_GLOBALcpsDecl,[U DO]),(program,[$]),
               (decl,[U DO,U SEMICOLON]),(opt_CHANGEMODE,[U IDENT]),
               (stoDecl,[U LOCAL,U GLOBAL,U DO,U SEMICOLON]),
               (opt_GLOBALglobImps,[U DO,U LOCAL]),
               (opt_LOCALcpsStoDecl,[U DO]),(funDecl,[U DO,U SEMICOLON]),
               (procDecl,[U DO,U SEMICOLON]),(rep_COMMAglobImp,[U DO,U LOCAL]),
               (globImps,[U DO,U LOCAL]),
               (opt_FLOWMODE,[U MECHMODE,U IDENT,U CHANGEMODE]),
               (globImp,[U DO,U LOCAL,U COMMA]),(rep_SEMICOLONdecl,[U DO]),
               (cpsDecl,[U DO]),(rep_SEMICOLONstoDecl,[U DO]),
               (cpsStoDecl,[U DO]),(rep_COMMAprogParam,[U RPAREN]),
               (opt_progParamrep_COMMAprogParam,[U RPAREN]),
               (progParamList,[U DO,U GLOBAL]),(progParam,[U RPAREN,U COMMA]),
               (rep_COMMAparam,[U RPAREN]),
               (opt_paramrep_COMMAparam,[U RPAREN]),
               (paramList,[U DO,U LOCAL,U GLOBAL,U RETURNS]),...],
       MM=[(opt_GLOBALcpsDecl,[(U GLOBAL,[[#,#]]),(U DO,[[]])]),
           (program,[(U PROGRAM,[[#,#,#,#,#,#,#]])]),
           (decl,
            [(U IDENT,[[#]]),(U CHANGEMODE,[[#]]),(U FUN,[[#]]),
             (U PROC,[[#]])]),
           (opt_CHANGEMODE,[(U CHANGEMODE,[[#]]),(U IDENT,[[]])]),
           (stoDecl,[(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]])]),
           (opt_GLOBALglobImps,
            [(U GLOBAL,[[#,#]]),(U DO,[[]]),(U LOCAL,[[]])]),
           (opt_LOCALcpsStoDecl,[(U LOCAL,[[#,#]]),(U DO,[[]])]),
           (funDecl,[(U FUN,[[#,#,#,#,#,#,#,#,#,#]])]),
           (procDecl,[(U PROC,[[#,#,#,#,#,#,#,#]])]),
           (rep_COMMAglobImp,[(U COMMA,[[#,#,#]]),(U DO,[[]]),(U LOCAL,[[]])]),
           (globImps,
            [(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]]),(U FLOWMODE,[[#,#]])]),
           (opt_FLOWMODE,
            [(U FLOWMODE,[[#]]),(U MECHMODE,[[]]),(U IDENT,[[]]),
             (U CHANGEMODE,[[]])]),
           (globImp,
            [(U IDENT,[[#,#,#]]),(U CHANGEMODE,[[#,#,#]]),
             (U FLOWMODE,[[#,#,#]])]),
           (rep_SEMICOLONdecl,[(U SEMICOLON,[[#,#,#]]),(U DO,[[]])]),
           (cpsDecl,
            [(U PROC,[[#,#]]),(U FUN,[[#,#]]),(U IDENT,[[#,#]]),
             (U CHANGEMODE,[[#,#]])]),
           (rep_SEMICOLONstoDecl,[(U SEMICOLON,[[#,#,#]]),(U DO,[[]])]),
           (cpsStoDecl,[(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]])]),
           (rep_COMMAprogParam,[(U COMMA,[[#,#,#]]),(U RPAREN,[[]])]),
           (opt_progParamrep_COMMAprogParam,
            [(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]]),(U FLOWMODE,[[#,#]]),
             (U RPAREN,[[]])]),(progParamList,[(U LPAREN,[[#,#,#]])]),
           (progParam,
            [(U IDENT,[[#,#,#]]),(U CHANGEMODE,[[#,#,#]]),
             (U FLOWMODE,[[#,#,#]])]),
           (rep_COMMAparam,[(U COMMA,[[#,#,#]]),(U RPAREN,[[]])]),
           (opt_paramrep_COMMAparam,
            [(U IDENT,[[#,#]]),(U CHANGEMODE,[[#,#]]),(U MECHMODE,[[#,#]]),
             (U FLOWMODE,[[#,#]]),(U RPAREN,[[]])]),
           (paramList,[(U LPAREN,[[#,#,#]])]),...],
       NULLABLE=[(opt_GLOBALcpsDecl,true),(program,false),(decl,false),
                 (opt_CHANGEMODE,true),(stoDecl,false),
                 (opt_GLOBALglobImps,true),(opt_LOCALcpsStoDecl,true),
                 (funDecl,false),(procDecl,false),(rep_COMMAglobImp,true),
                 (globImps,false),(opt_FLOWMODE,true),(globImp,false),
                 (rep_SEMICOLONdecl,true),(cpsDecl,false),
                 (rep_SEMICOLONstoDecl,true),(cpsStoDecl,false),
                 (rep_COMMAprogParam,true),
                 (opt_progParamrep_COMMAprogParam,true),(progParamList,false),
                 (progParam,false),(rep_COMMAparam,true),
                 (opt_paramrep_COMMAparam,true),(paramList,false),...],
       S=program,
       nonterms=[opt_GLOBALcpsDecl,program,decl,opt_CHANGEMODE,stoDecl,
                 opt_GLOBALglobImps,opt_LOCALcpsStoDecl,funDecl,procDecl,
                 rep_COMMAglobImp,globImps,opt_FLOWMODE,globImp,
                 rep_SEMICOLONdecl,cpsDecl,rep_SEMICOLONstoDecl,cpsStoDecl,
                 rep_COMMAprogParam,opt_progParamrep_COMMAprogParam,
                 progParamList,progParam,rep_COMMAparam,
                 opt_paramrep_COMMAparam,paramList,...],
       prods=[(opt_GLOBALcpsDecl,[[T GLOBAL,N cpsDecl],[]]),
              (program,
               [[T PROGRAM,T IDENT,N progParamList,N opt_GLOBALcpsDecl,T DO,
                 N cpsCmd,T ENDPROGRAM]]),
              (decl,[[N stoDecl],[N funDecl],[N procDecl]]),
              (opt_CHANGEMODE,[[T CHANGEMODE],[]]),
              (stoDecl,[[N opt_CHANGEMODE,N typedIdent]]),
              (opt_GLOBALglobImps,[[T GLOBAL,N globImps],[]]),
              (opt_LOCALcpsStoDecl,[[T LOCAL,N cpsStoDecl],[]]),
              (funDecl,
               [[T FUN,T IDENT,N paramList,T RETURNS,N stoDecl,
                 N opt_GLOBALglobImps,N opt_LOCALcpsStoDecl,T DO,N cpsCmd,
                 T ENDFUN]]),
              (procDecl,
               [[T PROC,T IDENT,N paramList,N opt_GLOBALglobImps,
                 N opt_LOCALcpsStoDecl,T DO,N cpsCmd,T ENDPROC]]),
              (rep_COMMAglobImp,[[T COMMA,N globImp,N rep_COMMAglobImp],[]]),
              (globImps,[[N globImp,N rep_COMMAglobImp]]),
              (opt_FLOWMODE,[[T FLOWMODE],[]]),
              (globImp,[[N opt_FLOWMODE,N opt_CHANGEMODE,T IDENT]]),
              (rep_SEMICOLONdecl,
               [[T SEMICOLON,N decl,N rep_SEMICOLONdecl],[]]),
              (cpsDecl,[[N decl,N rep_SEMICOLONdecl]]),
              (rep_SEMICOLONstoDecl,
               [[T SEMICOLON,N stoDecl,N rep_SEMICOLONstoDecl],[]]),
              (cpsStoDecl,[[N stoDecl,N rep_SEMICOLONstoDecl]]),
              (rep_COMMAprogParam,
               [[T COMMA,N progParam,N rep_COMMAprogParam],[]]),
              (opt_progParamrep_COMMAprogParam,
               [[N progParam,N rep_COMMAprogParam],[]]),
              (progParamList,
               [[T LPAREN,N opt_progParamrep_COMMAprogParam,T RPAREN]]),
              (progParam,[[N opt_FLOWMODE,N opt_CHANGEMODE,N typedIdent]]),
              (rep_COMMAparam,[[T COMMA,N param,N rep_COMMAparam],[]]),
              (opt_paramrep_COMMAparam,[[N param,N rep_COMMAparam],[]]),
              (paramList,[[T LPAREN,N opt_paramrep_COMMAparam,T RPAREN]]),...],
       terms=[NOT,LITERAL,MULTOPR,ADDOPR,RELOPR,BOOLOPR,INIT,DEBUGOUT,DEBUGIN,
              CALL,WHILE,ENDWHILE,IF,THEN,ELSE,ENDIF,BECOMES,SKIP,COLON,
              ATOMTYPE,MECHMODE,LPAREN,RPAREN,SEMICOLON,...]},fn),(fn,fn))
  : (term,?.X1,nonterm) ff'result
val it = () : unit
