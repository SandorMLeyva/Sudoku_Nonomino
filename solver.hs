module Solver where
    import Data.List
    import Nonomino
    import Point

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
