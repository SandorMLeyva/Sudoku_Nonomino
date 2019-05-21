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

n = Nonomino (Point 0 1 0) (Point 0 1 2) (Point 0 1 2) (Point 0 1 4) (Point 0 1 5) (Point 0 3 7) (Point 0 3 0) (Point 0 1 0) (Point 0 1 0)
n1 = Nonomino (Point 0 2 1) (Point 1 1 3) (Point 0 1 4) (Point 0 2 0) (Point 0 1 5) (Point 0 1 7) (Point 0 1 0) (Point 0 1 0) (Point 0 1 0)
n2 = Nonomino (Point 1 1 1) (Point 0 1 3) (Point 0 1 4) (Point 0 2 0) (Point 0 1 5) (Point 0 1 7) (Point 0 1 0) (Point 0 1 0) (Point 0 1 0)

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
nonoToListRow (nnl:nnls) i = [x | x <- nonoToList nnl, x_p(x) == i ] ++ nonoToListRow nnls i
-- Devuelve todos los elementos de una columna
nonoToListColumn [] _ = []
nonoToListColumn (nnl:nnls) i = [x | x <- nonoToList nnl, y_p(x) == i ] ++ nonoToListRow nnls i
