module Parser where

import Control.Monad
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.Expr
import Text.Parsec.String

import AST

parse location code = Parsec.parse expressions location code

expressions :: Parser [Expression]
expressions = sepEndBy1 expression terminator

expression :: Parser Expression
expression = do
    try (call)
    <|> try (assignStmt)
    <|> compoundExpr
    <|> localVar
    <|> literal

call :: Parser Expression
call = do
    name <- identifier
    char '('
    arg <- expression
    char ')'
    return $ Call name arg

assignStmt :: Parser Expression
assignStmt = do
    var <- identifier
    char '='
    expr <- expression
    return $ Assignment var expr

localVar :: Parser Expression
localVar = do
    name <- identifier
    return $ LocalVar name

compoundExpr = buildExpressionParser table term
    where
    table = [
        [Infix (char '*' >> return (BinOp Add)) AssocLeft],
        [Infix (char '/' >> return (BinOp Add)) AssocLeft],
        [Infix (char '+' >> return (BinOp Add)) AssocLeft],
        [Infix (char '-' >> return (BinOp Add)) AssocLeft]]
    term = literal <|> localVar

literal :: Parser Expression
literal = integerLiteral <|> stringLiteral
    where
    integerLiteral = do
        num <- many1 digit
        return $ Int (read num)

    stringLiteral = do
        char '"'
        s <- many (noneOf "\"")
        char '"'
        return $ String s

identifier :: Parser String
identifier = do
    start <- (letter <|> char '_')
    rest <- many (alphaNum <|> char '_')
    return $ start:rest

terminator = char '\n'
