module Parser (parse) where

import Control.Monad.Identity
import Text.Parsec hiding (parse)
import Text.Parsec.Expr

import Text.Parsec.IndentParsec.Combinator
import Text.Parsec.IndentParsec.Prim

import Lexer
import AST

parse location code = runIdentity $ runGIPT program () location code

program = whitespace >> statements

statements = many1 statement
statement = choice [defStatement,
                   returnStatement,
                   ifStatement,
                   try assignmentStatement,
                   expressionStatement]

ifStatement = do
    reserved "if"
    condition <- expression
    colon
    thenBlock <- blockOf statements
    elseBlock <- option [] (try elseClause)
    return $ If condition thenBlock elseBlock

    where
        elseClause = do
            reserved "else"
            colon
            blockOf statements

assignmentStatement = do
    variable <- identifier
    operator "="
    value <- expression
    return $ Assignment variable value

defStatement = do
    reserved "def"
    name <- identifier
    params <- parens (identifier `sepBy` comma)
    colon
    body <- blockOf statements
    return $ Def name params body

returnStatement = do
    reserved "return"
    e <- expression
    return $ Return e

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

        term = choice [try call, variable, literal]

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
            return $ Constant (String s)

        integerLiteral = do
            i <- integer
            return $ Constant (Int i)

