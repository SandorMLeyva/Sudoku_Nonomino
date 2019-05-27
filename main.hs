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
---------------------------------------------------------------------------------------

printUnsolvedSudoku = putStr (printBoard  boardEnd)
printSolution1 = putStr (printBoard   (solve1  boardEnd))
printSolution2 = putStr (printBoard  (solve2 boardEnd))






