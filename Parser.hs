module Parser (parse) where

import Control.Monad.Identity
import Text.Parsec hiding (parse)
import Text.Parsec.Expr

import Text.Parsec.IndentParsec.Combinator
import Text.Parsec.IndentParsec.Prim

import Lexer
import AST

parse location code = runIdentity $ runGIPT parser () location code

type Parser a = IndentParsecT String () Identity a

parser :: Parser [Statement]
parser = whitespace >> statements

statements = many1 statement
statement = choice [ifStatement, try assignmentStatement, expressionStatement]

ifStatement = do
    reserved "if"
    condition <- expression
    colon
    thenBlock <- blockOf statements
    return $ If condition thenBlock

assignmentStatement = do
    variable <- identifier
    operator "="
    value <- expression
    return $ Assignment variable value

expressionStatement = do
    e <- expression
    return $ Expression e

expression = buildExpressionParser table term
    where
        table = [
            [Infix (operator "*" >> return (BinOp Mul)) AssocLeft],
            [Infix (operator "/" >> return (BinOp Div)) AssocLeft],
            [Infix (operator "+" >> return (BinOp Add)) AssocLeft],
            [Infix (operator "-" >> return (BinOp Sub)) AssocLeft],
            [Infix (operator "!=">> return (BinOp NotEq)) AssocLeft],
            [Infix (operator "==">> return (BinOp Eq)) AssocLeft]]

        term = try call <|> variable <|> literal

        call = do
            name <- identifier
            arguments <- parens (expression `sepBy` comma)
            return $ Call name arguments

        variable = do
            name <- identifier
            return $ Variable name

        literal = integerLiteral <|> strLiteral

        strLiteral = do
            s <- stringLiteral
            return $ String s

        integerLiteral = do
            i <- integer
            return $ Int i

