# 		Sudoku Nonominó

##			 Proyecto de Haskell



### Detalles de implementación

La estructura del proyecto separa cada componente de la aplicación para su fácil entendimiento.

```d
main.hs
point.hs
nonomino.hs
builder.hs
solver.hs
printer.hs
test.hs
```

Para la solución del proyecto se implementaron 2 estructuras con el objetivo de representar el sudoku.

```haskell
data Point = Point {
        x_p :: Integer,
        y_p :: Integer,
        value :: Integer
        } deriving Show

data Nonomino = Nonomino {
        p0 :: Point,
        p1 :: Point,
        p2 :: Point,
        p3 :: Point,
        p4 :: Point,
        p5 :: Point,
        p6 :: Point,
        p7 :: Point,
        p8 :: Point
        } deriving Show
```

El sudoku no sería más que una lista de Nonominos.

Para crear un `nonominó`, al no conocer la posición que este ocupa en el sudoku, siempre hay que tomar como referencia el punto `(0,0)` y el primer valor de la instancia debe ser el punto más arriba a la izquierda.

Ligado a estas estructuras hay varias funciones, las cuales van a dar facilidad al trabajo con estas.

### Ideas para la solución del problema

Para la solución del sudoku, lo primero que se debe tener en cuenta es el orden de los `nonominós` ya que en dependencia de su figura estos tendrán un orden en el tablero. Para encontrar dicho orden se utiliza la función `tryOrder`, que recibe la lista de permutaciones de los `nonominos` y un sudoku base; devuelve una tupla en el que su primer elemento es una lista válida de nonominos y el segundo elemento es una lista con todas las posiciones iniciales para cada `nonominó` de la lista del primer elemento.

```haskell
 tryOrder:: [[Nonomino]] -> [Nonomino] -> ([Nonomino],[(Integer,Integer)])
 tryOrder [] board = ([],[])
 tryOrder (nonoList:xs) board = let (val, pos) = putFigure nonoList board [(0,0)] in 
 								if  val /= [] 
 								then (nonoList, filter (\x -> (fst x) /= (-1) && (snd x) /= (-1) ) pos) else tryOrder xs board

```

Después de encontrada la permutación válida, se procede a armar el `sudoku`. En el archivo `main.hs` podemos ver como se arma

```haskell
-- main.hs
boardEnd = map (\x -> nonominoFromList x) (moveNonomino (fst boardReady) (take 9 (snd boardReady )))
```

Llegado a este punto solo resta resolver el **sudoku** .

En el archivo `solvers.hs` se encuentran los algoritmos para resolver los `sudokus`, para su uso solo se debe escribir el nombre de la función y pasar como parámetro el sudoku sin resolver.

```haskell
sudokuSolved = solve2 boardEnd
```



### Modo de uso

Para usar la aplicación debe cargar `main.hs`, para resolver el con el **algoritmo 1**  o **algoritmo 2** se debe llamar a la función `printSolution1`  o `printSolution2`  respectivamente.

Esto solo le daría solución al sudoku de prueba definido en `test.hs`, por lo que si se quiere cambiar hay que seguir los siguientes pasos:

- En el archivo `test.hs` definir los **nonominos** de la siguiente forma:

  ```haskell
      n0 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 7) (Point 3 0 1) (Point 4 0 8) (Point 1 1 2) (Point 2 1 3) (Point 3 1 4) (Point 2 2 6)

  ```

  estos deben ser 9. **Siempre teniendo en cuenta que el primer punto es el que más arriba a la izquierda se encuentra en la figura definida**

- Después de definidos todos los **nonominós** construir una lista que los contenga a todos, sin importar el orden en que se pongan.

  ```haskell
      la = [n0,n1,n2,n3,n4,n5,n6,n8,n7]
  ```

  ​

- En el archivo `main.hs` en `perm = permutations la` poner el nombre de la variable que contiene la lista de nonominos, en este caso `la`



```haskell
-- main.hs

import Nonomino
import Point
import Builder
import Printer
import Solver
import Test
import Data.List

perm = permutations la
boardReady = tryOrder perm (buildEmptyBoard 0)
boardEnd = map (\x -> nonominoFromList x) (moveNonomino (fst boardReady) (take 9 (snd boardReady )))

printUnsolvedSudoku = putStr (printBoard  boardEnd)
printSolution1 = putStr (printBoard   (solve1  boardEnd))
printSolution2 = putStr (printBoard  (solve2 boardEnd))
```



