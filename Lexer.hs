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
    reservedNames = ["True", "False", "None", "if", "elif", "else", "def", "return", "while", "break", "pass", "assert"],
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
lexeme = IndentToken.lexeme lexer

integerLiteral = try (lexeme binInteger) <|> integer
    where
        binInteger = do
            _ <- char '0'
            _ <- oneOf "bB"
            num <- many1 (oneOf "01")
            return $ bin2dec num

        bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map (\c -> if c == '0' then 0 else 1)
