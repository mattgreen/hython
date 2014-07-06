module Parser where

import Control.Monad
import Text.Parsec
import Text.Parsec.Expr

import AST
import Lexer

parseCode code = parse program "(stuff)" code

program = whiteSpace >> expressions

expressions = sepEndBy1 expression terminator
expression = buildExpressionParser table terms

terms = try (liftM Int integer)
    <|> try (liftM String stringLiteral)
    <|> try (call)

call = do
    name <- identifier
    char '('
    expr <- expression
    char ')'
    return $ Call name expr

table = [
    [Infix (reservedOp "*" >> return (BinOp Mul)) AssocLeft],
    [Infix (reservedOp "/" >> return (BinOp Div)) AssocLeft],
    [Infix (reservedOp "+" >> return (BinOp Add)) AssocLeft],
    [Infix (reservedOp "-" >> return (BinOp Sub)) AssocLeft]]

terminator = lexeme (char '\n')
