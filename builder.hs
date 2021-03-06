module Builder where
    import Data.List
    import Nonomino
    import Point

    -- calcula las permutaciones de una lista
    permutations :: Eq a => [a] -> [[a]]
    permutations [] = [[]]
    permutations as = do a <- as
                         let l = delete a as
                         ls <- permutations l
                         return $ a : ls


    first:: [[(Integer,Integer)]] -> (Integer,Integer)
    first [] = (-1,-1)
    first ([]:xs) = first xs
    first (x:xs) = head x

    ---- calcula la posicion mas arriba a la izquierda que este disponible
    -- calcula la proxima posicion inicial en el que se va a ubicar la siguiente figura
    newPos:: [Nonomino] -> (Integer,Integer)
    newPos [] = (-1, -1)
    newPos board =  first [  [ (x_p y, y_p y)  | y <- (nonoToListRow board i), value y == (-1) ] | i <- [0..8]]

    -- Pone la figura a partir del punto que se especifica
    insertFigure:: [Point] -> [Nonomino] -> (Integer,Integer) -> [Nonomino]
    insertFigure [] board _ = board
    insertFigure (point:points) board (posX, posY) = let newpoint = addCord point (posX, posY) in
                                                    if (existValue newpoint board) || (x_p newpoint) > 8 || y_p newpoint > 8 || x_p newpoint < 0 || y_p newpoint < 0
                                                    then []
                                                    else insertFigure points (setValue newpoint board) (posX, posY)


    -- va por cada figura poniendo en la parte del nonomino con sus valores de x y correspondientes
    putFigure:: [Nonomino] -> [Nonomino] -> [(Integer,Integer)] -> ([Nonomino],[(Integer,Integer)])
    putFigure [] board pos = (board,pos)
    putFigure (fig:figs) board pos = let
                                        (posX, posY) = last pos
                                        firstPoint = p0 fig --asumiendo que el primer punto es el mas arriba a la izquierda
                                        result = insertFigure  (nonoToList fig) board (abs ( (x_p firstPoint) - posX  ),abs ((y_p firstPoint) - posY ))
                                    in
                                    if result /= []
                                    then putFigure figs result (pos ++ [newPos result])
                                    else ([],[])



    buildEmptyBoard:: Integer -> [Nonomino]
    buildEmptyBoard 9 = []
    buildEmptyBoard i = [nonominoFromList (map (\x -> Point x i (-1)) [0..8])] ++ buildEmptyBoard (i+1)




    -- esta funcion es la que trata de poner las figuras siempre lo mas arriba hacia la izquierda
    -- board es un sudoku de nonominos, pero al principio todos sus puntos tiene valor -1
    -- para representar que no hay valor o punto valido
    -- figure es un nonomino con los puntos que representan la figura

    -- va iterando por cada una de las permutaciones comprobando si sirve, en caso valida retorna el tablero
    tryOrder:: [[Nonomino]] -> [Nonomino] -> ([Nonomino],[(Integer,Integer)])
    tryOrder [] board = ([],[])
    tryOrder (nonoList:xs) board = let (val, pos) = putFigure nonoList board [(0,0)] in if  val /= [] then (nonoList,filter (\x -> (fst x) /= (-1) && (snd x) /= (-1) ) pos) else tryOrder xs board

--    moveNonomino:: [Point] -> [(Integer,Integer)] -> [Point]
    moveNonomino [] _ = []
    moveNonomino _ [] = []
    moveNonomino (x:xs) ((posX,posY):ps) = let
                                        posx = abs ((x_p (p0 x)) - posX)
                                        posy = abs ((y_p (p0 x)) - posY)
                                    in  (moveNonomino_ (nonoToList x) (posx,posy)):(moveNonomino xs ps)
                                    where
                                         moveNonomino_ [] _ = []
                                         moveNonomino_ (x:xs) (posX,posY) = (Point ((x_p x) + posX)  ((y_p x) + posY)  (value x)): moveNonomino_ xs (posX,posY)


