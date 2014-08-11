{
module Parser(parse) where
import Control.Monad.Error
import Data.Either
import Data.List

import Language
import qualified Lexer as L
}

%monad      {L.P}
%lexer      {L.lexer}{L.EOF}
%tokentype  {L.Token}
%error      { parseError }

%name parseTokens

%token
identifier  {L.Identifier $$}
literal     {L.Literal $$}
NEWLINE     {L.Newline}
"+"         {L.Operator "+"}
"-"         {L.Operator "-"}
"*"         {L.Operator "*"}
"/"         {L.Operator "/"}
"=="        {L.Operator "=="}
"!="        {L.Operator "!="}
"<"         {L.Operator "<"}
"<="        {L.Operator "<="}
">"         {L.Operator ">"}
">="        {L.Operator ">="}
"."         {L.Delimiter "."}
'('         {L.Delimiter "("}
')'         {L.Delimiter ")"}
':'         {L.Delimiter ":"}
'='         {L.Delimiter "="}
';'         {L.Delimiter ";"}
','         {L.Delimiter ","}
INDENT      {L.Indent}
DEDENT      {L.Dedent}

ASSERT      {L.Keyword "assert"}
BREAK       {L.Keyword "break"}
CLASS       {L.Keyword "class"}
CONTINUE    {L.Keyword "continue"}
DEF         {L.Keyword "def"}
DEL         {L.Keyword "del"}
ELIF        {L.Keyword "elif"}
ELSE        {L.Keyword "else"}
EXCEPT      {L.Keyword "except"}
FALSE       {L.Keyword "False"}
FINALLY     {L.Keyword "finally"}
FOR         {L.Keyword "for"}
FROM        {L.Keyword "from"}
GLOBAL      {L.Keyword "global"}
IF          {L.Keyword "if"}
IMPORT      {L.Keyword "import"}
IN          {L.Keyword "in"}
IS          {L.Keyword "is"}
LAMBDA      {L.Keyword "lambda"}
NONE        {L.Keyword "None"}
NONLOCAL    {L.Keyword "nonlocal"}
PASS        {L.Keyword "pass"}
RAISE       {L.Keyword "raise"}
RETURN      {L.Keyword "return"}
TRUE        {L.Keyword "True"}
TRY         {L.Keyword "try"}
WHILE       {L.Keyword "while"}
WITH        {L.Keyword "with"}
YIELD       {L.Keyword "yield"}
%%

or(p,q)
   : p      { $1 }
   | q      { $1 }

either(p,q)
   : p      { Left $1 }
   | q      { Right $1 }

opt(p)
   :        { Nothing }
   | p      { Just $1 }

rev_list1(p)
   : p               { [$1] }
   | rev_list1(p) p  { $2 : $1 }

many1(p)
   : rev_list1(p) { reverse $1 }

many0(p)
   : many1(p) { $1 }
   |         { [] }

sepOptEndBy(p,sep) 
   : sepByRev(p,sep) ',' { reverse $1 }
   | sepByRev(p,sep) { reverse $1 }

sepBy(p,sep): sepByRev(p,sep) { reverse $1 }

sepBy0(p,sep)
    :                   { [] }
    | sepBy(p,sep)      { $1 }

sepByRev(p,sep)
   : p { [$1] }
   | sepByRev(p,sep) sep p { $3 : $1 }

-- single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE
-- file_input: (NEWLINE | stmt)* ENDMARKER
file_input
    : many0(either(NEWLINE, stmt))  { foldl' (++) [] (rights $1) }

-- eval_input: testlist NEWLINE* ENDMARKER
-- 
-- decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
-- decorators: decorator+
-- decorated: decorators (classdef | funcdef)
-- funcdef: 'def' NAME parameters ['->' test] ':' suite
funcdef
    : DEF identifier parameters ':' suite { Def $2 $3 $5 }

-- parameters: '(' [typedargslist] ')'
parameters
    : '(' sepBy0(identifier, ',') ')' { $2 }

-- typedargslist: (tfpdef ['=' test] (',' tfpdef ['=' test])* [','
--        ['*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef]]
--      |  '*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef)
-- tfpdef: NAME [':' test]
-- varargslist: (vfpdef ['=' test] (',' vfpdef ['=' test])* [','
--        ['*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef]]
--      |  '*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef)
-- vfpdef: NAME

-- stmt: simple_stmt | compound_stmt
stmt
    : simple_stmt   { $1 }
    | compound_stmt { [$1] }

-- simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
simple_stmt
    : small_stmts opt(';') NEWLINE  { $1 }

small_stmts
    : small_stmt                    { [$1] }
    | small_stmts ';' small_stmt    { $3 : $1 }

-- small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
--              import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
small_stmt
    : expr_stmt     { $1 }
    | pass_stmt     { $1 }
    | flow_stmt     { $1 }
    | assert_stmt   { $1 }

-- expr_stmt: testlist_star_expr (augassign (yield_expr|testlist) |
--                      ('=' (yield_expr|testlist_star_expr))*)
expr_stmt
    : atom          { Expression $1 }

-- testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
-- augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
--             '<<=' | '>>=' | '**=' | '//=')
-- # For normal assignments, additional restrictions enforced by the interpreter
-- del_stmt: 'del' exprlist
-- pass_stmt: 'pass'
pass_stmt
    : PASS              { Pass }

-- flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
flow_stmt
    : break_stmt        { $1 }
    | continue_stmt     { $1 }
    | return_stmt       { $1 }

-- break_stmt: 'break'
break_stmt
    : BREAK             { Break }

-- continue_stmt: 'continue'
continue_stmt
    : CONTINUE          { Continue }

-- return_stmt: 'return' [testlist]
return_stmt
    : RETURN atom       { Return $2 }

-- yield_stmt: yield_expr
-- raise_stmt: 'raise' [test ['from' test]]
-- import_stmt: import_name | import_from
-- import_name: 'import' dotted_as_names
-- # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
-- import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
--               'import' ('*' | '(' import_as_names ')' | import_as_names))
-- import_as_name: NAME ['as' NAME]
-- dotted_as_name: dotted_name ['as' NAME]
-- import_as_names: import_as_name (',' import_as_name)* [',']
-- dotted_as_names: dotted_as_name (',' dotted_as_name)*
-- dotted_name: NAME ('.' NAME)*
-- global_stmt: 'global' NAME (',' NAME)*
-- nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
-- assert_stmt: 'assert' test [',' test]
assert_stmt
    : ASSERT sepBy(test, ',')        { Assert $ head $2 }

-- compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated
compound_stmt
    : if_stmt       { $1 }
    | while_stmt    { $1 }
    | funcdef       { $1 }
    | classdef      { $1 }

-- if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
if_stmt
    : IF test ':' suite many0(elif_clause) else_clause  { If ((IfClause $2 $4):$5) $6 }

elif_clause
    : ELIF test ':' suite { IfClause $2 $4 }

else_clause
    :                   { [] }
    | ELSE ':' suite    { $3 }

-- while_stmt: 'while' test ':' suite ['else' ':' suite]
while_stmt
    : WHILE test ':' suite      { While $2 $4 }

-- for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
-- try_stmt: ('try' ':' suite
--            ((except_clause ':' suite)+
--             ['else' ':' suite]
--             ['finally' ':' suite] |
--            'finally' ':' suite))
-- with_stmt: 'with' with_item (',' with_item)*  ':' suite
-- with_item: test ['as' expr]
-- # NB compile.c makes sure that the default except clause is last
-- except_clause: 'except' [test ['as' NAME]]

-- suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
suite
    : simple_stmt                       { $1 }
    | NEWLINE INDENT many1(stmt) DEDENT { concat $3 }

-- test: or_test ['if' or_test 'else' test] | lambdef
test
    : comparison { $1 }

-- test_nocond: or_test | lambdef_nocond
-- lambdef: 'lambda' [varargslist] ':' test
-- lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
-- or_test: and_test ('or' and_test)*
-- and_test: not_test ('and' not_test)*
-- not_test: 'not' not_test | comparison

-- comparison: expr (comp_op expr)*
comparison
    : atom                  { $1 }

-- # <> isn't actually a valid comparison operator in Python. It's here for the
-- # sake of a __future__ import described in PEP 401

-- comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
-- star_expr: '*' expr
-- expr: xor_expr ('|' xor_expr)*
-- xor_expr: and_expr ('^' and_expr)*
-- and_expr: shift_expr ('&' shift_expr)*
-- shift_expr: arith_expr (('<<'|'>>') arith_expr)*
-- arith_expr: term (('+'|'-') term)*
-- term: factor (('*'|'/'|'%'|'//') factor)*
-- factor: ('+'|'-'|'~') factor | power
-- power: atom trailer* ['**' factor]
-- atom: ('(' [yield_expr|testlist_comp] ')' |
--        '[' [testlist_comp] ']' |
--        '{' [dictorsetmaker] '}' |
--        NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
atom
    : identifier    { Variable $1 }
    | literal       { Constant $1 }
    | NONE          { Constant None }
    | TRUE          { Constant $ Bool True }
    | FALSE         { Constant $ Bool False }

-- testlist_comp: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )
-- trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
-- subscriptlist: subscript (',' subscript)* [',']
-- subscript: test | [test] ':' [test] [sliceop]
-- sliceop: ':' [test]
-- exprlist: (expr|star_expr) (',' (expr|star_expr))* [',']
-- testlist: test (',' test)* [',']
-- dictorsetmaker: ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
--                   (test (comp_for | (',' test)* [','])) )

-- classdef: 'class' NAME ['(' [arglist] ')'] ':' suite
classdef
    : CLASS identifier base_classes ':' suite { ClassDef $2 $3 $5 }

base_classes
    :                                   { [] }
    | '(' sepBy0(identifier, ',') ')'   { $2 }
-- 
-- arglist: (argument ',')* (argument [',']
--                          |'*' test (',' argument)* [',' '**' test] 
--                          |'**' test)
-- # The reason that keywords are test nodes instead of NAME is that using NAME
-- # results in an ambiguity. ast.c makes sure it's a NAME.
-- argument: test [comp_for] | test '=' test  # Really [keyword '='] test
-- comp_iter: comp_for | comp_if
-- comp_for: 'for' exprlist 'in' or_test [comp_iter]
-- comp_if: 'if' test_nocond [comp_iter]
-- 
-- # not used in grammar, but may appear in "node" passed from Parser to Compiler
-- encoding_decl: NAME
-- 
-- yield_expr: 'yield' [yield_arg]
-- yield_arg: 'from' test | testlist
{
tokenize code = L.tokenize code
parse code = L.evalP parseTokens code

parseError :: L.Token -> a
parseError t = error $ "Parse error: " ++ show t
}
