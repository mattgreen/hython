{
module Small(parse) where
import qualified Lexer as L
import Control.Monad.Error

import Language
}

%monad      {L.P}
%lexer      {L.lexer} {L.EOF}
%name       parseTokens
%tokentype  {L.Token}
%error      { parseError }

%token
literal     {L.Literal $$}
newline     {L.Newline}
":"         {L.Punctuation ":"}
indent      {L.Indent}
dedent      {L.Dedent}
LBRACE      {L.Punctuation "{"}
RBRACE      {L.Punctuation "}"}
while       {L.Keyword "while"}
%%

stmts
    : stmts newline stmt    { $3 : $1 }
    | stmts newline         { $1 }
    | stmt                  { [$1] }
    |                       { [] }

stmt
    : while_stmt            { $1 }
    | literal               { Expression (Constant $1) }

while_stmt
    : while literal ":" suite   { While (Constant $2) $4 }

suite
    : newline LBRACE stmts RBRACE { $3 }

{
tokenize code = L.tokenize code
parse code = L.evalP parseTokens code

parseError :: L.Token -> a
parseError t = error $ "Parse error: " ++ show t
}
