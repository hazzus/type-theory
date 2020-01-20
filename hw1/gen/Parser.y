{
module Parser where

import ParseTree
import Lexer
}

%name parseLambda
%tokentype { Token }
%error { parseError }
%monad { Either String } { >>= } { return }

%token
    '?'     { TokenLambda }
    '-'     { TokenDot }
    '('     { TokenLBrace }
    ')'     { TokenRBrace }
    VAR     { TokenVar $$ }
%%

Expression 
    : Application '?' VAR '-' Expression    { Apply $1 (Lambda $3 $5) }
    | '?' VAR '-' Expression                { Lambda $2 $4 }
    | Application                           { $1 }

Application
    : Application Atom                      { Apply $1 $2 }
    | Atom                                  { $1 }

Atom 
    : '(' Expression ')'                    { $2 }
    | VAR                                   { Var $1 }

{
parseError = fail "Parse error"
}
