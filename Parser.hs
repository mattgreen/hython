module Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.Expr
import Text.Parsec.String

import AST

parse = Parsec.parse program

program = sepEndBy1 statement terminator

statement :: Parser Statement
statement = try assignment <|> ifStatement <|> expr

assignment = do
    var <- identifier
    char '='
    expr <- expression
    return $ Assignment var expr

ifStatement = do
    string "if"
    char ' '
    guard <- expression
    char ':'
    terminator

    s <- statement
    return $ If guard s

expr = do
    e <- expression
    return $ Expression e

expression = buildExpressionParser table term
    where
        table = [
            [Infix (char '*' >> return (BinOp Mul)) AssocLeft],
            [Infix (char '/' >> return (BinOp Div)) AssocLeft],
            [Infix (char '+' >> return (BinOp Add)) AssocLeft],
            [Infix (char '-' >> return (BinOp Sub)) AssocLeft],
            [Infix (string "!=" >> return (BinOp NotEq)) AssocLeft],
            [Infix (string "==" >> return (BinOp Eq)) AssocLeft]]

        term = try call <|> literal <|> variable

call = do
    name <- identifier
    char '('
    arg <- expression
    char ')'
    return $ Call name arg

variable = do
    name <- identifier
    return $ Variable name

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

identifier = do
    start <- letter <|> char '_'
    rest <- many (alphaNum <|> char '_')
    return $ start:rest

terminator = char '\r' <|> char '\n'
