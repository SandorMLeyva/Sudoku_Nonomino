module Printer where
    import Nonomino
    import Point

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

    inNonomino:: Point -> [Point] -> Bool
    inNonomino _ [] = False
    inNonomino (Point x y v) ((Point x1 y1 _):ps) = (x == x1 && y == y1) || inNonomino (Point x y v) ps

    getColor:: Point -> [Nonomino] -> Integer -> String
    getColor point (nonomino:board) i = if point `inNonomino` (nonoToList nonomino) then idColor i else getColor point board (i+1)

    -- le da un formato a los elementos del board para verlos en pantalla
    normalize:: [Point] -> [Nonomino] -> String
    normalize [] _ = " \n"
    normalize ((Point x y (0)):ps) board = let color = getColor (Point x y 0) board 0 in  " " ++ color ++ "  " ++ normalize ps board
    normalize ((Point x y value):ps) board= let color = getColor (Point x y value) board 0 in " " ++ color ++ " "++ (show (value)) ++ normalize ps board

    --
    printBoard:: [Nonomino] -> String
    printBoard []  = "Configuracion de nonominos invalida"
    printBoard board  = printBoard_ board 0
                    where
                        printBoard_ _ 9 = ""
                        printBoard_ board i = (normalize (nonoToListRow board i) board) ++ printBoard_ board (i+1)

    -------------------------------End print board------------------------------------------
