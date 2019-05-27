module Point where

    data Point = Point {
        x_p :: Integer,
        y_p :: Integer,
        value :: Integer
    } deriving Show

    instance Eq Point where
            x == y = (x_p x == x_p y) && (y_p x == y_p y) && (value x == value y)


