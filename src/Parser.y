{
module Parser(parse) where
import Language
import qualified Lexer as L
import Control.Monad.Error
}

%monad{L.P}
%lexer{L.lexer}{L.EOF}
%name parse
%tokentype{L.Token}
%error { parseError }

%token
identifier  {L.Identifier $$}
literal     {L.Literal $$}
newline     {L.Newline}

%%

stmt : atom newline { $1 }

atom : identifier   { Variable $1 }
     | literal      { Constant $1 }

{
parseError :: L.Token -> a
parseError t = error $ "Parse error: " ++ show t
}
