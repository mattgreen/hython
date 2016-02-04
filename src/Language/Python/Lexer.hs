{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language.Python.Lexer
where

import Prelude hiding (exp, lex)

import Control.Monad
import Data.Complex
import Data.Char hiding (digitToInt)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec hiding (newline, tokens)

import Language.Python

data Token
    = Newline
    | Indent
    | Dedent
    | Identifier String
    | Keyword String
    | Operator String
    | Delimiter String
    | Literal Constant
    | StringLiteral Text
    deriving Show

type Tokens = [Token]

data LexerState = LexerState
                { lexerIndents :: [Int]
                , lexerImplicitJoins :: Int
                }

type Lexer a = Parsec Text LexerState a

lex :: Text -> Either ParseError Tokens
lex code = runParser program initialState "" newlineTerminatedCode
  where
    initialState = LexerState { lexerIndents = [0], lexerImplicitJoins = 0 }
    newlineTerminatedCode = T.snoc code '\n'

program :: Lexer Tokens
program = do
    tokens  <- many (choice [try blankLine, logicalLine])
    dedents <- remainingDedents
    return $ concat tokens ++ dedents
  where
    remainingDedents = do
        indents <- lexerIndents <$> getState
        return $ replicate (length indents - 1) Dedent

blankLine :: Lexer Tokens
blankLine = do
    skipMany (oneOf " \t\f")
    optional comment
    newline
    return []

logicalLine :: Lexer Tokens
logicalLine = do
    indent  <- indentation
    tokens  <- manyTill1 lexemes (try newline)
    return $ indent ++ concat tokens ++ [Newline]
  where
    lexemes = choice [ comment
                     , lexeme explicitLineJoin
                     , lexeme (try literal)
                     , lexeme (try keyword)
                     , lexeme identifier
                     , lexeme delimiterOrOperator
                     ]

    explicitLineJoin = do
        char '\\'
        newline
        return []

    lexeme p = do
        x   <- p
        ws  <- skippableWhitespace
        skipMany (oneOf ws)
        return x

    skippableWhitespace = do
        implicitJoin <- lexerImplicitJoins <$> getState
        if implicitJoin > 0
            then return " \t\f\r\n"
            else return " \t\f"

comment :: Lexer Tokens
comment = do
    char '#'
    manyTill anyChar (lookAhead newline)
    return []

indentation :: Lexer Tokens
indentation = do
    indentChars <- many (oneOf " \t")
    let level = calculateIndent indentChars

    indents <- fmap lexerIndents getState

    case indents of
        (x:_)   -> case compare level x of
            EQ  -> return []
            GT  -> do
                modifyState $ \s -> s { lexerIndents = level:indents }
                return [Indent]
            LT  -> do
                (removed, remaining) <- dedent level indents
                modifyState $ \s -> s { lexerIndents = remaining }
                return $ replicate (length removed) Dedent
        []      -> unexpected "indent stack should never be empty"
  where
    calculateIndent = foldl indentValue 0

    indentValue :: Int -> Char -> Int
    indentValue 0 '\t'      = 8
    indentValue acc '\t'    = 8 * ceiling ((fromIntegral acc / 8) :: Double)
    indentValue acc _       = acc + 1

    dedent level indents = do
        unless (level `elem` indents) $
            unexpected "unindent does not match any outer indentation level"

        return $ span (> level) indents

newline :: Lexer Tokens
newline = do
    try (string "\r\n") <|> string "\r" <|> string "\n"
    return [Newline]

delimiterOrOperator :: Lexer Tokens
delimiterOrOperator = choice [try longDelim, try longOp, shortOp, shortDelim]
  where
    longDelim = do
        let longDelims = ["+=", "-=", "*=", "**=", "/=", "//=", "%=", "&=",
                          "|=", "^=", ">>=", "<<="]
        d <- choice $ map (try . string ) longDelims
        return [Delimiter d]

    longOp = do
        let longOps = ["**", "//", "<<", ">>", "<=", ">=", "==", "!="]
        op <- choice $ map (try . string) longOps
        return [Operator op]

    shortOp = do
        c <- oneOf "+-*/%&|^~<>"
        return [Operator [c]]

    shortDelim = do
        c <- oneOf "()[]{},:.;@="

        when (c `elem` "([{") $
            modifyState $ \s -> s{ lexerImplicitJoins = lexerImplicitJoins s + 1 }

        when (c `elem` ")]}") $
            modifyState $ \s -> s{ lexerImplicitJoins = max 0 (lexerImplicitJoins s - 1) }

        return [Delimiter [c]]

identifier :: Lexer Tokens
identifier = do
    x   <- letter <|> char '_'
    xs  <- many (alphaNum <|> char '_')
    return [Identifier (x:xs)]

keyword :: Lexer Tokens
keyword = do
    kw  <- many1 (alphaNum <|> char '_')
    unless (kw `elem` keywords)
        parserZero
    return [Keyword kw]
  where
    keywords = ["False", "None", "True", "and", "as", "assert", "break", "class",
                "continue", "def", "del", "elif", "else", "except", "finally",
                "for", "from", "global", "if", "import", "in", "is", "lambda",
                "nonlocal", "not", "or", "pass", "raise", "return", "try",
                "while", "with", "yield"]

literal :: Lexer Tokens
literal = choice [try imaginaryLiteral, try floatLiteral, integerLiteral, stringLiteral]

integerLiteral :: Lexer Tokens
integerLiteral = do
    n   <- try binInt <|> try octInt <|> try hexInt <|> zeroDecInt <|> decInt
    return [Literal $ ConstantInt n]
  where
    binInt = do
        char '0'
        oneOf "bB"
        digits <- many1 digit
        return $ readBin digits

    zeroDecInt = do
        many1 (char '0')
        return 0

    decInt = do
        first   <- oneOf "123456789"
        digits  <- many digit
        return $ read (first:digits)

    octInt = do
        char '0'
        oneOf "oO"
        digits <- many1 digit
        return $ read ("0o" ++ digits)

    hexInt = do
        char '0'
        oneOf "xX"
        digits <- many1 hexDigit
        return $ read ("0x" ++ digits)

floatLiteral :: Lexer Tokens
floatLiteral = do
    s <- try exponentFloat <|> pointFloat
    return [Literal $ ConstantFloat (read s)]
  where
    exponentFloat = do
        leading <- try pointFloat <|> many1 digit
        oneOf "eE"
        sign    <- option '+' (oneOf "+-")
        exp     <- many1 digit
        return $ leading ++ "e" ++ [sign] ++ exp

    pointFloat = pointFloatLeading <|> pointFloatNoLeading
    pointFloatLeading = do
        leading <- many1 digit
        char '.'
        trailing <- option "0" (many1 digit)
        return $ leading ++ "." ++ trailing
    pointFloatNoLeading = do
        char '.'
        trailing <- many1 digit
        return $ "0." ++ trailing

imaginaryLiteral :: Lexer Tokens
imaginaryLiteral = do
    s <- try floatLit <|> many1 digit
    oneOf "jJ"
    return [Literal (ConstantImag (0.0 :+ read s))]
  where
    floatLit = do
        lit <- floatLiteral
        case lit of
            [Literal (ConstantFloat f)] -> return $ show f
            t                           -> fail $ "unexpected token from floatLiteral" ++ show t

stringLiteral :: Lexer Tokens
stringLiteral = try tripleQuotedString <|> singleQuotedString
  where
    singleQuotedString = do
        prefix      <- option '_' stringPrefix
        quote       <- char '"' <|> char '\''
        contents    <- manyTill stringChar (try (char quote))
        return [stringType prefix contents]

    tripleQuotedString = do
        prefix      <- option '_' stringPrefix
        quotes      <- string "\"\"\"" <|> string "'''"
        contents    <- manyTill stringChar (try (string quotes))
        return [stringType prefix contents]

    stringPrefix = oneOf "bB"

    stringType prefix s
      | 'B' == toUpper prefix       = Literal $ ConstantBytes s
      | otherwise                   = StringLiteral $ T.pack s

    stringChar = try escapedChar <|> anyChar
    escapedChar = do
        char '\\'
        c <- anyChar
        case lookup c escapeSequences of
            Just r  -> return r
            Nothing -> parserZero

    escapeSequences = [('"', '"'), ('\'', '\''), ('a', '\a'), ('b', '\b'),
                       ('f', '\f'), ('n', '\n'), ('r', '\r'), ('t', '\t'),
                       ('v', '\v')]

readBin :: Num b => String -> b
readBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0
  where
    digitToInt x = if x == '0' then 0 else 1

manyTill1
  :: Stream s m t =>
     ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill1 p end = do
    x   <- p
    xs  <- manyTill p end
    return (x:xs)
