module Lexer
where

import Control.Monad.Identity

import Text.Parsec
import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..))
import qualified Text.Parsec.IndentParsec.Token as IndentToken

definition = LanguageDef {
    commentStart = "",
    commentEnd = "",
    commentLine = "#",
    identStart = letter <|> char '_',
    identLetter = alphaNum <|> char '_',
    opStart = oneOf "-+/*=<>:",
    opLetter = oneOf "-+/*=<>:",
    reservedNames = ["if", "def", "return"],
    reservedOpNames = ["=", "+", "-", "*", "/", ":"],
    caseSensitive = True,
    nestedComments = False
}

lexer :: IndentToken.IndentTokenParser String () Identity
lexer = makeTokenParser definition

identifier = IndentToken.identifier lexer
reserved = IndentToken.reserved lexer
operator = IndentToken.reservedOp lexer
parens = IndentToken.parens lexer
integer = IndentToken.integer lexer
whitespace = IndentToken.whiteSpace lexer
comma = IndentToken.comma lexer
colon = operator ":"
stringLiteral = IndentToken.stringLiteral lexer
