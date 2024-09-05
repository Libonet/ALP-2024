{-
familia ::= tipo '->' familia | tipo
tipo ::= DInt | DChar | DFloat
-}

import Parsing
import Control.Applicative ((<|>))
import Data.Char


data Hasktype = DInt | DChar | DFloat | Fun Hasktype Hasktype deriving Show

familia = do t <- tipo
             recursivo t <|> return t

recursivo t = do symbol "->"
                 f <- familia
                 return (Fun t f)

tipo = tipoInt <|> tipoChar <|> tipoFloat

tipoInt = do symbol "Int"
             return (DInt)
 
tipoChar = do symbol "Char"
              return (DChar)

tipoFloat = do symbol "Float"
               return (DFloat)    