module Expression where

import           Data.IORef

data Expression
    = Apply Expression Expression
    | Lambda String Expression
    | Var String
    | Ref (IORef Expression)

