import Nonomino
import Point
import Builder
import Printer
import Solver
import Test
import Data.List
-----------------------------------------------------------------------------------------------------------------------------------


---------------------------------------------------------CREA TABLERO CON LOS NONOMINOS CORRESPONDIENTES---------------------------------------------------------------------------

--buildBoard (nonomino:xs) pos = (nonomino `setNonomino ` pos):

------------------------------------------------------------------------------------------------------------------------------------------------


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

ass = fst (ss  boardEnd)

printBoard2 = putStr (printBoard_  ass 0)
printBoard3 = putStr (printBoard_   (head (solve  boardEnd)) 0)






