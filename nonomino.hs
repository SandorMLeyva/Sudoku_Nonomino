module Nonomino where
    import Data.List
    import Point

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
