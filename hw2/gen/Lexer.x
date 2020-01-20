{
module Lexer where
}

%wrapper "basic"

$digits = 0-9
$chars = [a-z]

tokens :-
    $white+ ;
    \\                          { \s -> TokenLambda }
    "."                         { \s -> TokenDot }
    $chars [$chars $digits \']*  { \s -> TokenVar s }
    \(                          { \s -> TokenLBrace }
    \)                          { \s -> TokenRBrace }
{
data Token
    = TokenLambda
    | TokenDot
    | TokenLBrace
    | TokenRBrace
    | TokenVar String
    deriving (Eq, Show)

}
