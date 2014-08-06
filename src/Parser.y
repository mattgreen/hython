{
module Parser(parse) where
import Language
import qualified Lexer as L
import Control.Monad.Error
}

%monad{L.P}
%lexer{L.lexer}{L.EOF}
%name parseTokens
%tokentype{L.Token}
%error { parseError }

%token
identifier  {L.Identifier $$}
literal     {L.Literal $$}
newline     {L.Newline}
"."         {L.Punctuation "."}

%%

stmt : primary newline { $1 }

atom : identifier   { Variable $1 }
     | literal      { Constant $1 }

primary : atom          { $1 }
        | attributeRef  { $1 }

attributeRef : primary "." identifier { Attribute $1 $3 }

{
parse code = L.evalP parseTokens code

parseError :: L.Token -> a
parseError t = error $ "Parse error: " ++ show t
}
