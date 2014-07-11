{
module Parser where

import AST
import Lexer
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    identifier      { IdentifierToken $$ }
    '+'             { PlusToken }
    '='             { EqualsToken }
    int             { IntToken $$ }
    string          { StringToken $$ }
%%

Expression  : Assignment    { $1 }
            | Expression '+' Expression     { BinOp Add $1 $3 }
            | Literal       { $1 }

Assignment  : identifier '=' Expression    { Assignment $1 $3}

Literal     : int           { Int $1 }
            | string        { String $1 }

{
parse :: String -> Expression
parse code = parseTokens (alexLex code)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
