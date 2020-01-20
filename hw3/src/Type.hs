module Type where

data Type
    = Single String
    | Arrow Type Type
    deriving (Eq, Ord)

data TypeEquation
    = Equals Type Type
    deriving (Eq, Ord)

instance Show Type where
    show (Single s)  = s
    show (Arrow a b) =  "(" ++ show a ++ " -> " ++  show b ++ ")"

instance Show TypeEquation where
    show (Equals a b) = (show a) ++ "=" ++ (show b)

