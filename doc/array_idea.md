# Arrays in IML
## Deklaration und Instanzierung
`int` ist hier stellvertretend für alle möglichen variablen.
Das Array kann nur mit Variablen deklarierten typ befüllt werden.
```typescript
a2:int[100] 
```
Bei der Deklaration wird die Grösse in eine Variable gespeichert.
Hier wäre dies `size = 100`
Wertezuweisung an das Array a2 vom typ int[]
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
Eine zusätzliche Idee ist dass Arrays bei der Deklaration abgefüllt werden können
```typescript 
a:int[4] := [1,2,3,4];
```
### Syntax Sugar
Es soll möglich sein, bei einer direkten Zuweisung keine Grösse des Arrays anzugeben. Hierbei wird die Grösse des Zugewiesenen Arrays genommen.
```typescript
a:int[] := [1,2,3,4];
```
## Fehler
Bei einem Zugriff auf ein Element ausserhalb des deklarierten Bereichs wird zur Laufzeit ein `Index Out Of Bound` Fehler geworfen.
```typescript 
a2[100]; // out of bound error
```
## Technische Details
### Speicherallocation
Der Speicher muss linear alloziert werden.
Das Array benötigt hierbei zwei Parameter:`
    l_d := Länge des Datentyps sizeof(int)
    l_p := Länge des Arrays (Bsp Deklaration 100) 
    
    l_a = l_d * l_p 
## Datentyp
Es soll ein ArrayType erstellt werden in welchem die Länge des Arrays gespeichert ist.
```haskell
data ArrayType 
    = value: [DataType]
    | size: Int
```