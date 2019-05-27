module Test where
    import Nonomino
    import Point
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


    la = [n0,n1,n2,n3,n4,n5,n6,n8,n7]
    lb = [n0,n1,n2,n4,n3,n5,n6,n7,n8]


    -------------
    --  Definir los nonominos --
    --  una lista de nonominos representa un sudoku --
    --  crear una lista con los nonominos no necesariamente en el orden correcto
