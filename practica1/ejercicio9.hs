{-
declaration → type specifier declarator ’;’
declarator → ’*’ declarator | direct declarator

direct declarator -> (’(’ direct declarator ’)’ | identifier) dd'
dd' -> empty | ’[’ constant expression ’]’ dd'

type specifier → ’int’ | ’char’ | ’float’
constant expression → number
-}

import Parsing
import Control.Applicative ((<|>))
import Data.Char

data DType = DInt | DChar | DFloat
type ConstExpr = Char
data Dd' = E | Corchete ConstExpr Dd'
data Dd = Parentesis Dd Dd' | Id String Dd'

data Declarator = Pointer Declarator | Direct Dd
type Declaracion = DType Declarador


declaration :: Parser Declaracion
declaration = do tp <- type_specifier
                 decl <- declarator
                 symbol ";"
                 return (tp decl)

declarator = do symbol


