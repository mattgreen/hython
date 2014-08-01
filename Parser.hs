module Parser (parse) where

import Control.Monad.Identity
import Data.Complex
import Text.Parsec hiding (parse)
import Text.Parsec.Expr

import Text.Parsec.IndentParsec.Combinator
import Text.Parsec.IndentParsec.Prim

import Lexer
import AST

parse location code = runIdentity $ runGIPT program () location code

program = whitespace >> many statement

statements = many1 statement
statement = choice [defStatement,
                   classStatement,
                   returnStatement,
                   ifStatement,
                   whileStatement,
                   breakStatement,
                   continueStatement,
                   passStatement,
                   assertStatement,
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

classStatement = do
    reserved "class"
    name <- identifier
    bases <- option [] (parens (identifier `sepBy` comma))
    colon
    defs <- blockOf statements
    return $ ClassDef name bases defs

returnStatement = do
    reserved "return"
    e <- option (Constant None) expression
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

continueStatement = do
    reserved "continue"
    return Continue

passStatement = do
    reserved "pass"
    return Pass

assertStatement = do
    reserved "assert"
    e <- expression
    return $ Assert e

expressionStatement = do
    e <- expression
    return $ Expression e

expression = buildExpressionParser table term
    where
        table = [
            [Infix (operator "*" >> return (BinOp (ArithOp Mul))) AssocLeft],
            [Infix (operator "/" >> return (BinOp (ArithOp Div))) AssocLeft],
            [Infix (operator "+" >> return (BinOp (ArithOp Add))) AssocLeft],
            [Infix (operator "-" >> return (BinOp (ArithOp Sub))) AssocLeft],
            [Infix (operator "!=">> return (BinOp (BoolOp NotEq))) AssocLeft],
            [Infix (operator "==">> return (BinOp (BoolOp Eq))) AssocLeft],
            [Infix (operator "<" >> return (BinOp (BoolOp LessThan))) AssocLeft],
            [Infix (operator "<=">> return (BinOp (BoolOp LessThanEq))) AssocLeft],
            [Infix (operator ">" >> return (BinOp (BoolOp GreaterThan))) AssocLeft],
            [Infix (operator ">=">> return (BinOp (BoolOp GreaterThanEq))) AssocLeft]]

        term = choice [try call, try methodCall, try attribute, literal, variable, parenthesizedExpression]

        call = do
            name <- identifier
            arguments <- parens (expression `sepBy` comma)
            return $ Call name arguments

        methodCall = do
            receiver <- identifier
            _ <- char '.'
            name <- identifier
            arguments <- parens (expression `sepBy` comma)
            return $ MethodCall receiver name arguments

        attribute = do
            receiver <- identifier
            _ <- char '.'
            name <- identifier
            return $ Attribute receiver name

        variable = do
            name <- identifier
            return $ Variable name

        parenthesizedExpression = parens expression

        literal = choice [try imaginaryLiteral, try floatLiteral, intLiteral, strLiteral, trueLiteral, falseLiteral, noneLiteral]

        strLiteral = do
            s <- stringLiteral
            return $ Constant (String s)

        intLiteral = do
            i <- integerLiteral
            return $ Constant (Int i)

        floatLiteral = do
            i <- floatingPtLiteral
            return $ Constant (Float (read i))

        imaginaryLiteral = do
            i <- imaginaryNumberLiteral
            return $ Constant (Imaginary (0.0 :+ read i))

        trueLiteral = do
            reserved "True"
            return $ Constant (Bool True)

        falseLiteral = do
            reserved "False"
            return $ Constant (Bool False)

        noneLiteral = do
            reserved "None"
            return $ Constant None

