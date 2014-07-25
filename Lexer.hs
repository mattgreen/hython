{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

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
    reservedNames = ["True", "False", "None", "if", "elif", "else", "def", "return", "while", "break", "continue", "pass", "assert"],
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

floatingPtLiteral = do
    s <- lexeme (try exponentFloat <|> float)
    return $ read s

  where
    fractionOnly = do
        char '.'
        fraction <- many1 digit
        return $ "0." ++ fraction

    leadingOnly = do
        int <- many1 digit
        char '.'
        return $ int ++ ".0"

    leadingAndFraction = do
        int <- many1 digit
        char '.'
        fraction <- many1 digit
        return $ int ++ "." ++ fraction

    float = try leadingAndFraction <|> fractionOnly <|> leadingOnly

    exponentFloat = do
        num <- try float <|> many1 digit
        oneOf "eE"
        sign <- option '+' (oneOf "+-")
        rest <- many1 digit
        return $ num ++ "e" ++ [sign] ++ rest
