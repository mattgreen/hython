{
module Parser(parse) where
import qualified Lexer as L
import Control.Monad.Error

import Language
}

%monad{L.P}
%lexer{L.lexer}{L.EOF}
%name parseTokens
%tokentype{L.Token}
%error { parseError }

%token
identifier  {L.Identifier $$}
literal     {L.Literal $$}
newline     {L.Newline}
"."         {L.Punctuation "."}
"("         {L.Punctuation "("}
")"         {L.Punctuation ")"}
":"         {L.Punctuation ":"}
","         {L.Punctuation ","}
"="         {L.Punctuation "="}
indent      {L.Indent}
dedent      {L.Dedent}
assert      {L.Keyword "assert"}
break       {L.Keyword "break"}
class       {L.Keyword "class"}
continue    {L.Keyword "continue"}
def         {L.Keyword "def"}
elif        {L.Keyword "elif"}
else        {L.Keyword "else"}
if          {L.Keyword "if"}
pass        {L.Keyword "pass"}
return      {L.Keyword "return"}
while       {L.Keyword "while"}
%%

program
    :                   { [] }
    | statements        { $1 }

statements
    : statement             { [$1] }
    | statements newline statement     { $1 ++ [$3] }
    | statements newline { $1 }
    | newline statements    { $2 }
    | newline           { [] }

statement
    : assignment        { $1 }
    | assertion         { $1 }
    | breakStatement    { $1 }
    | classStatement    { $1 }
    | continueStatement { $1 }
    | defStatement      { $1 }
    | ifStatement       { $1 }
    | passStatement     { $1 }
    | returnStatement   { $1 }
    | whileStatement    { $1 }
    | expression        { Expression $1 }

assertion
    : assert expressionList { Assert $2 }

assignment
    : identifier "=" expressionList { Assignment (Variable $1) $3 }

breakStatement
    : break             { Break }

classStatement
    : class identifier ":" suite                    { ClassDef $2 [] $4 }
    | class identifier "(" parameterList ")" suite  { ClassDef $2 $4 $6 }

continueStatement
    : continue          { Continue }

defStatement
    : def identifier "(" parameterList ")" ":" suite { Def $2 $4 $7 }

ifStatement
    : if expression ":" suite elseClause    { If [(IfClause $2 $4)] $5 }

elseClause
    :                           { [] }
    | else ":" suite            { $3 }

passStatement
    : pass              { Pass }

returnStatement
    : return                { Return $ Constant None }
    | return expressionList { Return $2 }

whileStatement
    : while expression ":" suite    { While $2 $4 }

expressionList
    : expression        { $1 }

expression
    : primary   { $1 }

suite
    : newline indent statements { $3 }

parameterList
    :               { [] }
    | parameters    { $1 }

parameters
    : parameter                 { [$1] }
    | parameter "," parameters  { $1:$3 }

parameter
    : identifier                { $1 }

atom
    : identifier    { Variable $1 }
    | literal       { Constant $1 }

primary
    : atom          { $1 }
    | attributeRef  { $1 }
    | call          { $1 }

attributeRef
    : primary "." identifier    { Attribute $1 $3 }

call
    : primary "(" argumentList ")"  { Call $1 $3 }

argumentList
    :               { [] }
    | arguments     { $1 }

arguments
    : argument                  { [$1] }
    | argument "," arguments    { $1:$3 }

argument
    : primary                   { $1 }

{
parse code = L.evalP parseTokens code

parseError :: L.Token -> a
parseError t = error $ "Parse error: " ++ show t
}
