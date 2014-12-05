{
{-# OPTIONS_GHC -w #-}

module Language.Python.Lexer where

import Codec.Binary.UTF8.String (encode)
import Control.Monad.State
import Data.Complex
import Data.List
import Data.Word
import Text.Regex

import Language.Python.Core
}

$digit = 0-9
$space = [\ ]
$content = [^ $space \#]
$newline = \n
$delimiters = [\( \) \[ \] \{ \} \, \: \. \; \@ \=]

@operator   = "+" | "-" | "*" | "**" | "/" | "//" | "%" | "<<" | ">>" | "&"
            | "|" | "^" | "~" | "<"  | ">" | "<=" | ">=" | "==" | "!="

@delimiter  = "("  | ")" | "[" | "]" | "{" | "}" | ","  | ":"  | "."
            | ";"  | "@" | "=" | "->" | "+=" | "-=" | "*=" | "/=" | "//="
            | "%=" | "&=" | "|=" | "^=" | ">>=" | "<<=" | "**="

@keywordOrIdentifier = [a-zA-Z_][a-zA-Z0-9_]*

@intpart = $digit+
@fraction = "." $digit+
@exponent = [eE] [\+\-]? $digit+
@pointfloat = @intpart @fraction | @fraction | @intpart "."
@exponentfloat = (@intpart | @pointfloat) @exponent
@floatnumber = @pointfloat | @exponentfloat

@imagnumber = (@floatnumber | @intpart) [jJ]

@stringescapeseq = \\ [a-zA-Z]

@shortstringcharsingle = [^\\\']
@shortstringitemsingle = @shortstringcharsingle | @stringescapeseq

@shortstringchardouble = [^\\\"]
@shortstringitemdouble = @shortstringchardouble | @stringescapeseq
@shortstring = ' @shortstringitemsingle* ' | \" @shortstringitemdouble* \"

@longstringchar = [^\\] | $white
@longstringitem = @longstringchar | @stringescapeseq
@longstring = ''' @longstringitem* ''' | \"\"\" @longstringitem* \"\"\"

@stringprefix = [rRuU]
@stringliteral = @stringprefix? (@shortstring | @longstring)

tokens :-
    -- Whitespace handling
    $newline $space* / $content { handleIndentation }
    $newline                    ;
    $space+                     ;

    -- Comments
    "#".*                       ;

    -- Integers
    0+                          { \_ s -> return $ Literal (Int 0) }
    [1-9][0-9]*                 { \_ s -> return $ Literal (Int $ read s) }
    0[bB][01]+                  { \_ s -> return $ Literal (Int $ binaryToDecimal s) }
    0[oO][0-9]+                 { \_ s -> return $ Literal (Int $ read s) }
    0[xX][0-9a-fA-F]+           { \_ s -> return $ Literal (Int $ read s) }

    -- Floats
    @floatnumber                { \_ s -> return $ Literal (Float $ read (legalizeFloat s)) }

    -- Imaginary numbers
    @imagnumber                 { \_ s -> return $ Literal (Imaginary (0.0 :+ (read (legalizeImag s)))) }

    -- Strings
    @stringliteral              { \_ s -> return $ StringLiteral (stringContent s) }

    @keywordOrIdentifier        { \_ s -> return $ keywordOrIdentifier s }

    @delimiter                  { \_ s -> return $ Delimiter s }
    @operator                   { \_ s -> return $ Operator s }
{
data Token
     = Newline
     | Indent
     | Dedent
     | Identifier String
     | Keyword String
     | Literal Object
     | StringLiteral String
     | Operator String
     | Delimiter String
     | EOF
     deriving (Eq, Show)

keywordOrIdentifier :: String -> Token
keywordOrIdentifier s
    | s `elem` keywords = Keyword s
    | otherwise         = Identifier s
  where
    keywords = ["False", "None", "True", "and", "as", "assert", "break", "class",
                "continue", "def", "del", "elif", "else", "except", "finally",
                "for", "from", "global", "if", "import", "in", "is", "lambda",
                "nonlocal", "not", "or", "pass", "raise", "return", "try",
                "while", "with", "yield"]

binaryToDecimal :: String -> Integer
binaryToDecimal s = convert $ drop 2 s
  where
    convert = foldr (\c s -> s * 2 + c) 0 . reverse . map (\c -> if c == '0' then 0 else 1)

convertEscapeSequences :: String -> String
convertEscapeSequences = id

legalizeFloat :: String -> String
legalizeFloat s = killSpaces (foldl' legalize s replacements)
  where
    killSpaces str = filter(/=' ') str
    legalize str (regex, replacement) = subRegex (mkRegex regex) str replacement
    replacements = [("^\\.", "0."),
                    ("([^0-9])\\.", "\\1 0."),
                    ("\\.$", ".0"),
                    ("\\.([^0-9])", ".0\\1")]

legalizeImag :: String -> String
legalizeImag s = init (legalizeFloat s)

stripQuotes :: String -> String
stripQuotes s = drop quoteLength (take ((length s) - quoteLength) s)
  where
    quoteLength
      | take 3 s == "\"\"\""    = 3
      | otherwise               = 1

stringContent :: String -> String
stringContent s = convertEscapeSequences $ stripQuotes s

-- The functions that must be provided to Alex's basic interface
-- The input: last character, unused bytes, remaining string
data AlexInput = AlexInput Char [Word8] String
     deriving Show
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput c (b:bs) s) = Just (b,AlexInput c bs s)
alexGetByte (AlexInput _ [] [])    = Nothing
alexGetByte (AlexInput _ [] (c:s)) = case encode [c] of
                             	   	(b:bs) -> Just (b, AlexInput c bs s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput c _ _) = c

-- Our state

data ParseState = 
     ParseState {alexInput::AlexInput,
                 indent_stack::[Int],
                 pending_tokens::[Token]}
                 deriving Show

initialState::String -> ParseState
initialState s = ParseState {   alexInput = AlexInput '\n' [] s,
                                indent_stack = [1],
                                pending_tokens = []
                                }

-- Our Parser monad
type P a = State ParseState a

evalP::P a -> String -> a
evalP m s= evalState m (initialState s)

-- Set input

handleIndentation :: Int -> String -> P Token
handleIndentation newIndent _ = do
    s <- get
    let indents@(currentIndent:_) = indent_stack s

    when (newIndent > currentIndent) $ do
        put s{indent_stack = newIndent:indents, pending_tokens = [Indent]}
    when (newIndent < currentIndent) $ do
        let (pre,post@(top:_)) = span (> newIndent) indents
        if top == newIndent
          then
            put s{indent_stack = post, pending_tokens = map (const Dedent) pre}
          else
            error $ "Incorrect indentation, expected " ++ (show top)

    return Newline

-- Action to read a token
readToken::P Token
readToken = do
          s <- get
          case pending_tokens s of
               t:ts -> do
			put s{pending_tokens = ts}
			return t  
               [] ->  case alexScan (alexInput s) 0 of
                       AlexEOF -> do
                                    let dedents = map (const Dedent) (init (indent_stack s))
                                    put s{pending_tokens=(pending_tokens s)++dedents++[EOF]}
                                    return Newline
                       AlexError _ -> error "!Lexical error"
                       AlexSkip inp' _ -> do    
                          put s{alexInput = inp'}
                          readToken
                       AlexToken inp' n act -> do 
                          let (AlexInput _ _ buf) = alexInput s
                          put s{alexInput = inp'}
                          act n (take n buf)

readTokens :: P [Token]
readTokens = do
    token <- readToken
    case token of
        EOF -> do
            s <- get
            return [token]
        _   -> do
            rest <- readTokens
            return (token:rest)

tokenize :: String -> [Token]
tokenize s = evalP readTokens s

lexer :: (Token -> P a) -> P a
lexer cont = readToken >>= cont

}
