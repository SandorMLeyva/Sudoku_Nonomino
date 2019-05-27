module Solver where
    import Data.List
    import Nonomino
    import Point

    -- devuelve la lista de posiciones vacias en el tablero
    emptyPoints:: [[Point]] -> [Point]
    emptyPoints [] = []
    emptyPoints (x:xs) = emptyPoints_ x ++ emptyPoints xs
                        where
                            emptyPoints_ [] = []
                            emptyPoints_ ((Point x y 0):ps) = (Point x y 0): emptyPoints_ ps
                            emptyPoints_ ((Point x y _):ps) = emptyPoints_ ps

    -- dice si poner el punto en el tablero es valido
    isValid:: Point -> [Nonomino] -> Bool
    isValid (Point x y v) board =  validBoard (setValue (Point x y v) board)

    solve1,solve2:: [Nonomino] -> [Nonomino]
    -- Calcula todos las posibles soluciones del sudoku, pero solo nos quedamos con la primera
    -- Calcula las posiciones vacias en el tablero
    -- por cada posicion vacia busca los posibles valores validos para esa posicion
    -- ...
    solve1 board = head (solve_1_ (emptyPoints ([ nonoToList x | x <- board ])) board)
                where
                    solve_1_ [] board = [board]
                    solve_1_ ((Point x y value):xs) board = concatMap (solve_1_ xs) candidatesBoards
                        where
                            candidatesValues = [v |v <- [1..9], isValid (Point x y v) board ] --impl isValid
                            candidatesBoards = map (\z -> setValue (Point x y z) board) candidatesValues

    -- calcula todas las posiciones vacias en el sudoku
    -- en cada posicion vacia calcula los posibles valores que pueden estar
    -- pone el punto y llama recursivo
    -- si no encuentra mas posiciones vacias termina
    solve2 board = let (b, solution) = solve_2 board (emptyPoints ([ nonoToList x | x <- board ]))
                    in if solution then b else []
                    where
                        solve_2 board [] = (board, True)
                        solve_2 board ((Point x y _):emptys) = solve_2_ [ Point x y i | i <- [1..9], isValid (Point x y i) board ] board emptys
                                                where
                                                    solve_2_ [] board (vx:vxs) = (board, False)
                                                    solve_2_ (validPoint:vps) board emptys =
                                                        let result = solve_2 (setValue validPoint board) emptys
                                                        in if snd result then  result else solve_2_ vps board emptys
