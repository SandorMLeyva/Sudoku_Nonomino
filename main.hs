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


p = Point 0 1
o = Point 0 2

n = Nonomino (Point 0 0 1) (Point 0 1 1) (Point 0 2 1) (Point 0 3 0) (Point 0 4 0) (Point 0 5 0) (Point 0 6 0) (Point 0 7 0) (Point 0 8 0)
n1 = Nonomino (Point 1 0 1) (Point 1 1 1) (Point 1 2 1) (Point 1 3 0) (Point 1 4 0) (Point 1 5 0) (Point 1 6 0) (Point 1 7 0) (Point 1 8 0)
n2 = Nonomino (Point 2 0 1) (Point 2 1 2) (Point 2 2 3) (Point 2 3 4) (Point 2 4 5) (Point 2 5 6) (Point 2 6 7) (Point 2 7 8) (Point 2 8 0)
--n1 = Nonomino (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 0) (Point 1 1 0) (Point 1 1 0) (Point 1 1 0) (Point 1 1 0) (Point 1 1 0)
--n2 = Nonomino (Point 1 1 10) (Point 0 1 3) (Point 0 1 4) (Point 0 2 0) (Point 7 3 5) (Point 0 1 7) (Point 0 1 0) (Point 0 3 9) (Point 0 1 0)

ll = [n,n1,n2]

-- Convierte un nonomino en una lista de puntos
nonoToList :: Nonomino -> [Point]
nonoToList n = [p0 n,p1 n,p2 n,p3 n, p4 n,p5 n,p6 n,p7 n,p8 n]
-- Dice si un nonomino es valido (si no tiene numero que se repitan)
--nonominoValido :: [Point] -> [Int] -> Bool
nonominoValido [] _ = True
nonominoValido ((Point _ _ value):nns) l = if value == 0 || value `elem` l  then nonominoValido nns [x | x <- l, x /= value || x == 0] else False



-- Devuelve todos los elementos de una fila
--nonoToListRow, nonoToListColumn :: [Nonomino] -> Int -> [Point]
nonoToListRow [] _ = []
nonoToListRow (nnl:nnls) i = [x | x <- nonoToList nnl, y_p x == i ] ++ nonoToListRow nnls i
-- Devuelve todos los elementos de una columna
nonoToListColumn [] _ = []
nonoToListColumn (nnl:nnls) i = [x | x <- nonoToList nnl, x_p x == i ] ++ nonoToListColumn nnls i

-- funcion auxiliar para saber si dos puntos son iguales en caso de que se este comparando con un punto
-- que no tiene valor igual de True
nonoSameValue (Point _ _ v1) (Point _ _ v2) = v1 == v2 && (v1 /= 0 || v2 /= 0)

-- rotrna true si todas las filas y todas las columna estan bien (no tiene numeros repetidos)
nonoAllRowOk nonomino = (==9) `all` map (\x -> length (nonoSameValue `nubBy` nonoToListRow nonomino x)) [0..8]
nonoAllColumnOk nonomino = (==9) `all` map (\x -> length (nonoSameValue `nubBy` nonoToListColumn nonomino x)) [0..8]

















