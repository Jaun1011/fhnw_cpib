# Arrays in IML

## Zwischenbericht

Jan Kuonen, Ismail Cadaroski

Compilerbau, HS2021, Team CK

Beide Studierenden beteiligen sich gleichermassen am Projekt.

---


## Abstract

## Idee der Erweiterung mit konkreten Beispielen

## Lexikalischer Syntax

## Grammatikalischer Syntax

### Prüfung durch Fix&Foxi

## Kontext- und Typeinschränkung

## Vergleich mit anderen Programmiersprachen

## Enwurfsalternativen

---

## Deklaration und Instanzierung

`int` ist hier stellvertretend für alle möglichen variablen.
Das Array kann nur mit Variablen deklarierten typ befüllt werden.

```typescript
a2:int[100] 
```

Bei der Deklaration wird die Grösse in eine Variable gespeichert.
Hier wäre dies `size = 100`


Wertezuweisung an das Array `a2` vom Typ `int[]`

```typescript 
a2[0] := 1;
a2[99] := 2;
```

Die Wertezuweisung muss jeweils zur Laufzeit geprüft werden. Hierbei muss das Objekt ArrayType in JVM Code übersetzt werden.

## Zugriff

Um auf ein Element im Array zugreifen zu können

```typescript 
print(a2[0]); // 1
```

## Schnellzuweisung (Optional)

Eine zusätzliche Idee ist, dass Arrays bei der Deklaration abgefüllt werden können. Falls die beiden Anzahlen nicht übereinstimmen, wird zur Laufzeit ein Fehler geworfen.

```typescript 
a:int[4] := [1,2,3,4];
```

### Syntax Sugar
Es soll möglich sein, bei einer Deklaration keine Grösse des Arrays anzugeben. Hierbei wird die Grösse des zugewiesenen Arrays genommen.

```typescript
a:int[] := [1,2,3,4];
```

## Fehlerbehandlung

Bei einem Zugriff auf ein Element ausserhalb des deklarierten Bereichs wird zur **Laufzeit** ein `Index Out Of Bound` Fehler geworfen. 

```typescript 
a1[99]; // => null
a2[100]; // out of bound error
```
"Null" ist hierbei die Zahl 0 welche beim Erstellen des Arrays in alle Blöcke gefüllt wird.

## Technische Details
### Speicherallocation

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


## Datentyp

Es soll ein ArrayType erstellt werden, in welchem die Länge des Arrays gespeichert ist und jene Länge zum Datentyp gehört. 

```haskell
data ArrayType 
    = value: [DataType]
    | size: Int
```


## Quellenverzeichnis