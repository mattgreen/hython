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

%%

atom : identifier   { Variable $1 }
     --| literal      { Constant $1 }

{
parseError _ = throwError "!Parse error"
}
