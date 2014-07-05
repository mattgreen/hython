module Lexer where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token as P

pythonDef :: LanguageDef s
pythonDef = emptyDef {
    P.commentLine = "#",
    P.identStart = letter <|> oneOf "_$",
    P.identLetter = alphaNum <|> oneOf "_$",
    P.opStart = P.opLetter pythonDef,
    P.opLetter = oneOf "+-*/<=>&|!",
    P.caseSensitive = True
}

python = makeTokenParser pythonDef

integer = P.integer python
lexeme = P.lexeme python
identifier = P.identifier python
stringLiteral = P.stringLiteral python
reservedOp = P.reservedOp python
operator = P.operator python
whiteSpace = P.whiteSpace python
semi = P.semi python
