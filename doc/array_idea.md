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