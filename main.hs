import Data.List

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

instance Eq Point where
        x == y = (x_p x == x_p y) && (y_p x == y_p y) && (value x == value y)

instance Eq Nonomino where
        x == y = (p0 x == p0 y) && (p1 x == p1 y) && (p2 x == p2 y) && (p3 x == p3 y) && (p4 x == p4 y) && (p5 x == p5 y) && (p6 x == p6 y) && (p7 x == p7 y) && (p8 x == p8 y)
addCord (Point x1 y1 v1) (x2, y2) = Point (x1+x2) (y1+y2) v1

-- Crea un nonomino a partir de una lista de puntos
nonominoFromList nonomino = Nonomino (nonomino !! 0) (nonomino !! 1) (nonomino !! 2) (nonomino !! 3) (nonomino !! 4) (nonomino !! 5) (nonomino !! 6) (nonomino !! 7) (nonomino !! 8)

-- Convierte un nonomino en una lista de puntos
nonoToList :: Nonomino -> [Point]
nonoToList n = [p0 n,p1 n,p2 n,p3 n, p4 n,p5 n,p6 n,p7 n,p8 n]

-- Pone el valor del punto en la lista de puntos que representa un nonomino
setValueInList _ [] = []
setValueInList point1 (point2:points)  = if x_p point1 == x_p point2 && y_p point1 == y_p point2 then point1:points else point2:setValueInList point1 points

-- recorre la lista de nonominos para buscar el punto que hay que sustituir,
-- retorna una lista de nonominos con el punto cambiado
setValue _ [] = []
setValue point (nonomino:nonominos) = nonominoFromList(point `setValueInList` nonoToList nonomino): setValue point nonominos

-- poner aqui que x y sean iguales y que los v sean distintos de -1
--samePoint (Point x1 y1 v1) (Point x2 y2 v2) = x1 == x2 && y1 == y2 &&  v2 /= -1
samePoint (Point x1 y1 v1) nonomino = or [ x1 == x2 && y1 == y2 &&  v2 /= -1 | Point x2 y2 v2 <- nonoToList nonomino]

-- dice si un punto existe en la lista de nonominos
existValue _ [] = False
existValue point (n:nonominos) = if samePoint point n then True else existValue point nonominos
-----------------------------------------------------------------------------------------------------------------------------------------------
--Complementos para trabajar con los nonominos






-- Dice si un nonomino es valido (si no tiene numero que se repitan)
--nonominoValid :: [Point] -> [Int] -> Bool
nonominoValid [] _ = True
nonominoValid ((Point _ _ value):nns) l = if value == 0 || value `elem` l  then nonominoValid nns [x | x <- l, x /= value || x == 0] else False

-- comprueba que todos los nonominos son validos
allNonominoValid [] = True
allNonominoValid (nonomino:nonominos) =  (nonoToList nonomino  `nonominoValid` [1..9]) && allNonominoValid nonominos

compareRow a b  = compare (x_p a) (x_p b)
compareColumn a b  = compare (y_p a) (y_p b)

-- Devuelve todos los elementos de una fila
--nonoToListRow, nonoToListColumn :: [Nonomino] -> Int -> [Point]
nonoToListRow [] _ = []
nonoToListRow (nnl:nnls) i = sortBy compareRow ([x | x <- nonoToList nnl, y_p x == i ] ++ nonoToListRow nnls i)
-- Devuelve todos los elementos de una columna
nonoToListColumn [] _ = []
nonoToListColumn (nnl:nnls) i = sortBy compareColumn ([x | x <- nonoToList nnl, x_p x == i ] ++ nonoToListColumn nnls i)

-- funcion auxiliar para saber si dos puntos son iguales en caso de que se este comparando con un punto
-- que no tiene valor igual de True
nonoSameValue (Point _ _ v1) (Point _ _ v2) = v1 == v2 && (v1 /= 0 || v2 /= 0)

-- rotrna true si todas las filas y todas las columna estan bien (no tiene numeros repetidos)
nonoAllRowOk nonomino = (==9) `all` map (\x -> length (nonoSameValue `nubBy` nonoToListRow nonomino x)) [0..8]
nonoAllColumnOk nonomino = (==9) `all` map (\x -> length (nonoSameValue `nubBy` nonoToListColumn nonomino x)) [0..8]

-- Dice si el tablero es valido verificando las filas las culumnas y los nonominos
validBoard nonomino = allNonominoValid nonomino && nonoAllColumnOk nonomino && nonoAllRowOk nonomino


-----------------------------------------------------------------------------------------------------------------------------------

-- TODO poner un filtro que si el nonomino no tiene el punto (0,0) no comprobar las combinaciones en la q sea el primero !! ENTENDER BIEN EL CODIGO
-- calcula las permutaciones de una lista
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations as = do a <- as
                     let l = delete a as
                     ls <- permutations l
                     return $ a : ls

first_ (x:_) =  x

first [] = (-1,-1)
first ([]:xs) = first xs
first (x:xs) = first_ x

---- calcula la posicion mas arriba a la izquierda que este disponible
-- calcula la proxima posicion inicial en el que se va a ubicar la siguiente figura
newPos [] = (-1, -1) -- TODO: tener en cuenta ese valor de retorno, pq puede dar problemas
newPos board =  first [  [ (x_p y, y_p y)  | y <- (nonoToListRow board i), value y == (-1) ] | i <- [0..8]]

-- Pone la figura a partir del punto que se especifica
insertFigure [] board _ = board
insertFigure (point:points) board (posX, posY) = let newpoint = addCord point (posX, posY) in
                                                if existValue newpoint board
                                                then []
                                                else insertFigure points (setValue newpoint board) (posX, posY)


-- va por cada figura poniendo en la parte del nonomino con sus valores de x y correspondientes
-- TODO ver el caso en que newPos retorna (-1,-1), nunca va a llegar a eso
putFigure [] board pos = (board,pos)
putFigure (fig:figs) board pos = let
                                    (posX, posY) = last pos
                                    firstPoint = p0 fig --asumiendo que el primer punto es el mas arriba a la izquierda
                                    result = insertFigure  (nonoToList fig) board (abs ( (x_p firstPoint) - posX  ),abs ((y_p firstPoint) - posY ))
                                in
                                if result /= []
                                then putFigure figs result (pos ++ [newPos result])
                                else ([],[])




buildEmptyBoard 9 = []
buildEmptyBoard i = [nonominoFromList (map (\x -> Point x i (-1)) [0..8])] ++ buildEmptyBoard (i+1)


-------------------------------Start print board------------------------------------------
idColor 0 = "\x1b[41m"
idColor 1 = "\x1b[42m"
idColor 2 = "\x1b[43m"
idColor 3 = "\x1b[44m"
idColor 4 = "\x1b[45m"
idColor 5 = "\x1b[46m"
idColor 6 = "\x1b[41m"
idColor 7 = "\x1b[42m"
idColor 8 = "\x1b[43m"

inNonomino _ [] = False
inNonomino (Point x y v) ((Point x1 y1 _):ps) = (x == x1 && y == y1) || inNonomino (Point x y v) ps

getColor point (nonomino:board) i = if point `inNonomino` (nonoToList nonomino) then idColor i else getColor point board (i+1)

-- le da un formato a los elementos del board para verlos en pantalla
normalize [] _ = " |\n"
normalize ((Point x y (-1)):ps) board = let color = getColor (Point x y (-1))  board 0 in  color ++ " |  " ++ normalize ps board
normalize ((Point x y (0)):ps) board = let color = getColor (Point x y 0) board 0 in  color ++ " |  " ++ normalize ps board
normalize ((Point x y value):ps) board= let color = getColor (Point x y value) board 0 in " | "++ color ++(show (value)) ++ normalize ps board

--
printBoard_ _ 9 = ""
printBoard_ board i = (normalize (nonoToListRow board i) board) ++ printBoard_ board (i+1)
--                    printBoard board (i+1)

-- imprime board

-------------------------------End print board------------------------------------------

-- esta funcion es la que trata de poner las figuras siempre lo mas arriba hacia la izquierda
-- board es un sudoku de nonominos, pero al principio todos sus puntos tiene valor -1
-- para representar que no hay valor o punto valido
-- figure es un nonomino con los puntos que representan la figura
--tryOrder (figure:figres) board = let result = insertFigure $ nonoList figure in if length result /= 0 then


-- va iterando por cada una de las permutaciones comprobando si sirve, en caso valida retorna el tablero
--tryOrder (nonoList:xs) board pos = let val = putFigure nonoList board pos in if  val /= [] then val else tryOrder xs val
tryOrder [] board = ([],[])
tryOrder (nonoList:xs) board = let (val, pos) = putFigure nonoList board [(0,0)] in if  val /= [] then (nonoList,filter (\x -> (fst x) /= (-1) && (snd x) /= (-1) ) pos) else tryOrder xs board

-- recibe una lista de lista de puntos(nonominos) y una tupla (x,y) devuelve el nonomino con los puntos corridos
--(abs ( (x_p firstPoint) - posX  ),abs ((y_p firstPoint) - posY ))
moveNonomino_ [] _ = []
moveNonomino_ (x:xs) (posX,posY) = (Point ((x_p x) + posX)  ((y_p x) + posY)  (value x)): moveNonomino_ xs (posX,posY)

moveNonomino [] _ = []
moveNonomino _ [] = []
moveNonomino (x:xs) ((posX,posY):ps) = let
                                    posx = abs ((x_p (p0 x)) - posX)
                                    posy = abs ((y_p (p0 x)) - posY)
                                in  (moveNonomino_ (nonoToList x) (posx,posy)):(moveNonomino xs ps)
---------------------------------------------------------CREA TABLERO CON LOS NONOMINOS CORRESPONDIENTES---------------------------------------------------------------------------

--buildBoard (nonomino:xs) pos = (nonomino `setNonomino ` pos):

------------------------------------------------------------------------------------------------------------------------------------------------

------------------------- TEST ------------------------

-- test1
--n0 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 0 1 0) (Point 1 1 0) (Point 2 1 0) (Point 0 2 0) (Point 1 2 0) (Point 2 2 0)
--n1 = Nonomino (Point 3 0 0) (Point 3 1 0) (Point 3 2 0) (Point 3 3 0) (Point 4 3 0) (Point 2 3 0) (Point 1 3 0) (Point 0 3 0) (Point 0 4 0)
--n2 = Nonomino (Point 2 0 0) (Point 2 1 0) (Point 2 2 0) (Point 3 2 0) (Point 3 3 0) (Point 3 4 0) (Point 2 4 0) (Point 1 4 0) (Point 0 4 0)
------
--n3 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 0 1 0) (Point 1 1 0) (Point 2 1 0) (Point 1 2 0) (Point 3 0 0) (Point 3 1 0)
--n4 = Nonomino (Point 1 0 0) (Point 1 1 0) (Point 0 1 0) (Point 0 2 0) (Point 1 2 0) (Point 2 0 0) (Point 2 1 0) (Point 2 2 0) (Point 2 3 0)
--n5 = Nonomino (Point 1 0 0) (Point 1 1 0) (Point 0 1 0) (Point 2 1 0) (Point 3 1 0) (Point 4 1 0) (Point 5 1 0) (Point 6 1 0) (Point 7 1 0)
----
--n6 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 0 1 0) (Point 1 1 0) (Point 2 1 0) (Point 0 2 0) (Point 1 2 0) (Point 2 2 0)
--n7 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 3 0 0) (Point 4 0 0) (Point 5 0 0) (Point 5 1 0) (Point 5 2 0) (Point 4 2 0)
--n8 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 3 0 0) (Point 4 0 0) (Point 0 1 0) (Point 1 1 0) (Point 2 1 0) (Point 3 1 0)

------test2
--n0 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 3 0 1) (Point 4 0 0) (Point 1 1 0) (Point 2 1 0) (Point 3 1 0) (Point 2 2 0)
--n1 = Nonomino (Point 1 0 2) (Point 2 0 0) (Point 3 0 3) (Point 4 0 0) (Point 0 1 0) (Point 1 1 0) (Point 2 1 0) (Point 3 1 9) (Point 4 1 0)
--n2 = Nonomino (Point 0 0 6) (Point 0 1 0) (Point 0 2 8) (Point 1 1 0) (Point 1 2 0) (Point 2 2 0) (Point 2 3 4) (Point 3 2 0) (Point 3 1 0)
------
--n3 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 3 0 8) (Point 4 0 0) (Point 3 1 0) (Point 3 2 0) (Point 4 1 0) (Point 4 2 0)
--n4 = Nonomino (Point 3 0 7) (Point 3 1 0) (Point 2 1 5) (Point 2 2 0) (Point 1 2 2) (Point 1 3 9) (Point 1 4 1) (Point 1 5 0) (Point 0 5 0)
--n5 = Nonomino (Point 1 0 4) (Point 2 0 0) (Point 1 1 6) (Point 2 1 0) (Point 0 2 0) (Point 1 2 0) (Point 2 2 0) (Point 2 3 0) (Point 2 4 0)
----
--n6 = Nonomino (Point 0 0 0) (Point 0 1 0) (Point 0 2 3) (Point 0 3 4) (Point 0 4 0) (Point 1 0 0) (Point 1 1 0) (Point 1 2 0) (Point 1 3 0)
--n7 = Nonomino (Point 1 0 0) (Point 2 0 0) (Point 1 1 0) (Point 2 1 0) (Point 1 2 0) (Point 2 2 0) (Point 1 3 7) (Point 2 3 0) (Point 0 3 0)
--n8 = Nonomino (Point 0 0 8) (Point 1 0 0) (Point 2 0 5) (Point 0 1 7) (Point 1 1 0) (Point 2 1 9) (Point 0 2 3) (Point 1 2 6) (Point 2 2 0)

------ test3
--n0 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 7) (Point 3 0 1) (Point 4 0 8) (Point 1 1 2) (Point 2 1 3) (Point 3 1 4) (Point 2 2 6)
--n1 = Nonomino (Point 1 0 2) (Point 2 0 0) (Point 3 0 0) (Point 4 0 4) (Point 0 1 0) (Point 1 1 8) (Point 2 1 7) (Point 3 1 9) (Point 4 1 5)
--n2 = Nonomino (Point 0 0 6) (Point 0 1 7) (Point 0 2 8) (Point 1 1 1) (Point 1 2 0) (Point 2 2 0) (Point 2 3 4) (Point 3 2 0) (Point 3 1 0)
------
--n3 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 3 0 8) (Point 4 0 0) (Point 3 1 0) (Point 3 2 0) (Point 4 1 0) (Point 4 2 0)
--n4 = Nonomino (Point 3 0 7) (Point 3 1 0) (Point 2 1 5) (Point 2 2 0) (Point 1 2 0) (Point 1 3 9) (Point 1 4 1) (Point 1 5 0) (Point 0 5 0)
--n5 = Nonomino (Point 1 0 4) (Point 2 0 0) (Point 1 1 6) (Point 2 1 0) (Point 0 2 0) (Point 1 2 0) (Point 2 2 0) (Point 2 3 0) (Point 2 4 0)
----
--n6 = Nonomino (Point 0 0 0) (Point 0 1 0) (Point 0 2 3) (Point 0 3 4) (Point 0 4 0) (Point 1 0 0) (Point 1 1 0) (Point 1 2 0) (Point 1 3 0)
--n7 = Nonomino (Point 1 0 0) (Point 2 0 0) (Point 1 1 0) (Point 2 1 0) (Point 1 2 0) (Point 2 2 0) (Point 1 3 7) (Point 2 3 0) (Point 0 3 0)
--n8 = Nonomino (Point 0 0 8) (Point 1 0 4) (Point 2 0 5) (Point 0 1 7) (Point 1 1 2) (Point 2 1 9) (Point 0 2 3) (Point 1 2 6) (Point 2 2 1)

---- test4
n0 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 7) (Point 3 0 1) (Point 4 0 8) (Point 1 1 2) (Point 2 1 3) (Point 3 1 4) (Point 2 2 6)
n1 = Nonomino (Point 1 0 0) (Point 2 0 0) (Point 3 0 0) (Point 4 0 4) (Point 0 1 0) (Point 1 1 8) (Point 2 1 7) (Point 3 1 9) (Point 4 1 5)
n2 = Nonomino (Point 0 0 6) (Point 0 1 7) (Point 0 2 8) (Point 1 1 1) (Point 1 2 0) (Point 2 2 0) (Point 2 3 4) (Point 3 2 0) (Point 3 1 0)
----
n3 = Nonomino (Point 0 0 0) (Point 1 0 0) (Point 2 0 0) (Point 3 0 8) (Point 4 0 0) (Point 3 1 0) (Point 3 2 0) (Point 4 1 0) (Point 4 2 0)
n4 = Nonomino (Point 3 0 7) (Point 3 1 0) (Point 2 1 5) (Point 2 2 0) (Point 1 2 0) (Point 1 3 9) (Point 1 4 1) (Point 1 5 0) (Point 0 5 0)
n5 = Nonomino (Point 1 0 0) (Point 2 0 0) (Point 1 1 6) (Point 2 1 0) (Point 0 2 0) (Point 1 2 0) (Point 2 2 0) (Point 2 3 0) (Point 2 4 0)
--
n6 = Nonomino (Point 0 0 0) (Point 0 1 0) (Point 0 2 3) (Point 0 3 4) (Point 0 4 0) (Point 1 0 0) (Point 1 1 0) (Point 1 2 0) (Point 1 3 0)
n7 = Nonomino (Point 1 0 0) (Point 2 0 0) (Point 1 1 0) (Point 2 1 0) (Point 1 2 0) (Point 2 2 0) (Point 1 3 7) (Point 2 3 0) (Point 0 3 0)
n8 = Nonomino (Point 0 0 8) (Point 1 0 0) (Point 2 0 5) (Point 0 1 0) (Point 1 1 2) (Point 2 1 9) (Point 0 2 3) (Point 1 2 6) (Point 2 2 1)


la = [n0,n1,n2,n3,n4,n5,n6,n7,n8]
lb = [n0,n1,n2,n4,n3,n5,n6,n7,n8]

pp = permutations la

--
--printBoard = putStr (printBoard_ (fst (putFigure la (buildEmptyBoard 0) [(0,0)])) 0)
--
---- lista de nonomino en el orden en que se van a poner

---------------------Aqui queda el sudoku listo para resolverlo-----------------------
q = tryOrder pp (buildEmptyBoard 0)                                                 --
qw1 = fst q                                                                         --
qw0 = take 9 (snd q )                                                               --
nonoFinal = moveNonomino qw1 qw0
boardEnd = map (\x -> nonominoFromList x) nonoFinal
printBoard = putStr (printBoard_  boardEnd 0)
---------------------------------------------------------------------------------------
emptyPoints_ [] = []
emptyPoints_ ((Point x y 0):ps) = (Point x y 0): emptyPoints_ ps
emptyPoints_ ((Point x y _):ps) = emptyPoints_ ps

-- recibe la lista de nonominos (los nonominos en forma de lista)
emptyPoints [] = []
emptyPoints (x:xs) = emptyPoints_ x ++ emptyPoints xs

isValid (Point x y v) board =  validBoard (setValue (Point x y v) board)

-- recibe el board , es decir la lista de nonominos
--solve board = emptyPoints ([ nonoToList x | x <- board ])
solve board = solve_ (emptyPoints ([ nonoToList x | x <- board ])) board
            where
                solve_ [] board = [board]
                solve_ ((Point x y value):xs) board = concatMap (solve_ xs) candidatesBoards
                    where
                        candidatesValues = [v |v <- [1..9], isValid (Point x y v) board ] --impl isValid
                        candidatesBoards = map (\z -> setValue (Point x y z) board) candidatesValues
--

-- esto es lo q se le pasa para la variable empty = emptyPoints ([ nonoToList x | x <- board ])
ss board = solve2 board (emptyPoints ([ nonoToList x | x <- board ]))
solve2 board [] = (board, True)
solve2 board ((Point x y _):emptys) = solve2_ [ Point x y i | i <- [1..9], isValid (Point x y i) board ] board emptys
                        where
                            solve2_ [] board (vx:vxs) = (board, False)
                            solve2_ (validPoint:vps) board emptys =
                                let result = solve2 (setValue validPoint board) emptys
                                in if snd result then  result else solve2_ vps board emptys
ass = fst (ss  boardEnd)

printBoard2 = putStr (printBoard_  ass 0)
printBoard3 = putStr (printBoard_   (head (solve  boardEnd)) 0)






