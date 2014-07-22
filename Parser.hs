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
                   whileStatement,
                   breakStatement,
                   passStatement,
                   try assignmentStatement,
                   expressionStatement]

ifStatement = do
    ifBlock <- ifClause "if"
    elifBlocks <- many (ifClause "elif")
    elseBlock <- option [] (try elseClause)
    return $ If (ifBlock : elifBlocks) elseBlock

    where
        ifClause keyword = do
            reserved keyword
            condition <- expression
            colon
            block <- blockOf statements
            return $ IfClause condition block

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

whileStatement = do
    reserved "while"
    e <- expression
    colon
    loopBlock <- blockOf statements
    return $ While e loopBlock

breakStatement = do
    reserved "break"
    return Break

passStatement = do
    reserved "pass"
    return Pass

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

        term = choice [try call, literal, variable]

        call = do
            name <- identifier
            arguments <- parens (expression `sepBy` comma)
            return $ Call name arguments

        variable = do
            name <- identifier
            return $ Variable name

        literal = choice [intLiteral, strLiteral, trueLiteral, falseLiteral, noneLiteral]

        strLiteral = do
            s <- stringLiteral
            return $ Constant (String s)

        intLiteral = do
            i <- integerLiteral
            return $ Constant (Int i)

        trueLiteral = do
            reserved "True"
            return $ Constant (Bool True)

        falseLiteral = do
            reserved "False"
            return $ Constant (Bool False)

        noneLiteral = do
            reserved "None"
            return $ Constant None

