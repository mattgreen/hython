{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$newline = [\r\n]
$operator = [\+ \- \* \/ \=]

tokens :-
    "#".*                               ;
    " "+                                ;
    [\t]                                { \s -> IndentToken }
    $newline                            { \s -> NewlineToken }
    "+"                                 { \s -> PlusToken }
    "="                                 { \s -> EqualsToken }
    $digit+                             { \s -> IntToken (read s) }
    $alpha [$alpha $digit \_]*          { \s -> IdentifierToken s }

{
data Token =
      IntToken Integer
    | StringToken String
    | IdentifierToken String
    | PlusToken
    | EqualsToken
    | IndentToken
    | DedentToken
    | NewlineToken
    deriving (Eq, Show)

alexLex code = alexScanTokens code
}

