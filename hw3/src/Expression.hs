module Expression where

data Expression
    = Apply Expression Expression
    | Var String
    | Lambda String Expression
    deriving (Eq, Ord)

instance Show Expression where
    show (Lambda s exp) = "(\\" ++ s ++ "." ++ (show exp) ++ ")"
    show (Apply f s)    = "(" ++ show f ++ " " ++ show s ++ ")"
    show (Var s)        = s
