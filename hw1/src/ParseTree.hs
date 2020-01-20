module ParseTree where

data ParseTree
    = Apply ParseTree ParseTree
    | Var String
    | Lambda String ParseTree

instance Show ParseTree where
    show (Lambda s exp) = "(\\" ++ s ++ "." ++ (show exp) ++ ")"
    show (Apply f s)    = "(" ++ show f ++ " " ++ show s ++ ")"
    show (Var s)        = s
