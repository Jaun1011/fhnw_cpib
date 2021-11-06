# Arrays in IML

## Zwischenbericht

Jan Kuonen, Ismail Cadaroski

Compilerbau, HS2021, Team CK

Beide Studierenden beteiligen sich gleichermassen am Projekt.

---

## Abstract
Das Ziel dieser Erweiterung ist es, den Compiler durch folgende Elemente auszubauen:
- Lexikalische Erweiterung und Repräsentation von Arrays
- Den Array mit Variablen des deklarierten types befüllen
- Die grösse des Arrays nicht bei der Deklaration zuweisen zu müssen.

## Idee der Erweiterung mit konkreten Beispielen
### Deklaration und Instanziierung

`int` ist hier stellvertretend für alle möglichen variablen.
Das Array kann nur mit Variablen deklarierten typ befüllt werden.

```typescript
a2:int[100] 
```

Bei der Deklaration wird die Grösse in eine Variable gespeichert.
Hier wäre dies `size = 100`


Die Wertezuweisung muss jeweils zur Laufzeit geprüft werden. Hierbei muss das Objekt ArrayType in JVM Code übersetzt werden.


### Fehlerbehandlung

Bei einem Zugriff auf ein Element ausserhalb des deklarierten Bereichs wird zur **Laufzeit** ein `Index Out Of Bound` Fehler geworfen. 

```typescript 
a1[99]; // => null
a2[100]; // out of bound error
```
"Null" ist hierbei die Zahl 0 welche beim Erstellen des Arrays in alle Blöcke gefüllt wird.

### Technische Details
#### Speicherallocation

Der Speicher muss linear alloziert werden.
Das Array benötigt hierbei zwei Parameter:`


    l_d := Länge des Datentyps sizeof(int)
    l_p := Länge des Arrays (Bsp Deklaration 100) 
    
    l_a = l_d * l_p 


**Beispiel**

Es wird eine Variable `a`, mit dem Typ `int`, als Array mit der Länge `10` deklariert.

```typescript
a:int[10];
```
Daraus folgt folgende Kalkulation

    d := sizeof(int) = 32
    p := size_array = 10
    s := d * p = 32 * 10 = 320

---


## Lexikalischer Syntax

Arrays sollen über folgende Repräsentation zulässig sein:
    
    
    <arraytype> ::= <primtype> '[' <intlit> ']';

Beipiele für zulässige Arrays wären:

Deklaration ohne Grössenangabe:
```typescript
a:int[] := [1,2,3,4];
```
Deklaration mit Grössenangabe:
```typescript 
a:int[4] := [1,2,3,4];
```

Typendeklaration:

Der Typ wird lexikalisch als arraytype deklariert, welche als Subtyp des <type> gilt und somit die Grammatik wie folgt erweitert:

```typescript
<type> ::= <primtype> | <arraytype>;

<primtype> ::= <booltype> | <inttype>;
<booltype> ::= bool;
<inttype> ::= int32 | int64 | int1024;

<arraytype> ::= <primtype> '[' <intlit> ']';
```
Wertezuweisung an das Array `a2` vom Typ `int[]`

```typescript 
a2[0] := 1;
a2[99] := 2;
```

---
## Grammatikalischer Syntax
Grammatikalisch geschahen ausserhalb der Aufbaustruktur des Arrays keine Änderungen.

---
### Prüfung durch Fix&Foxi
Fix&Foxi bestätigt, dass die Grammatik vollständig ist.

```hs
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
---
## Vergleich mit anderen Programmiersprachen

### Arrays in Haskell:

In Haskell werden Arrays als Abstrakte Datentypen behandelt. Die Array Erstellungsfunktion Formt einen Array aus einem Paar von Bounds und einer Liste von Index-Value-Paaren (Associative Liste).
Ein Beispiel hierfür wäre:
```typescript
array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
```

### Arrays in Java:

In Java wird der Array als Container Objekt mit einer fixen Anzahl eines einzigen Typs vorausgesetzt, was unsere Implementation gleicht. Jedoch wird es in unserer Umsetzung möglich sein Arrays auch ohne eine vordefinierte Grösse zu Deklarieren. 

Beispiel zur Umsetzung in Java:
```typescript
// declares an array of integers
int[] anArray;

// allocates memory for 10 integers
anArray = new int[10];
```

---

## Quellenverzeichnis
1) The Java Tutorials, "Arrays", [Online, abgerufen am 01.11.2021]. Verfügbar unter : https://docs.oracle.com/javase/tutorial/java/nutsandbolts/arrays.html

2) A Gentle Introduction to Haskell, Version 98, "13 Arrays", [Online, abgerufen am 01.11.2021]. Verfügbar unter: https://www.haskell.org/tutorial/arrays.html