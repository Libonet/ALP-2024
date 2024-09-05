import Parsing
import Control.Applicative ((<|>))
import Data.Char

data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype]

familiafuncional str = case parse familia str of
                        [(res, [])] -> Just res
                        _ -> Nothing

familia = do sepBy tipo (symbol "->")

tipo = tipoInt <|> tipoChar <|> tipoFloat

tipoInt = do symbol "Int"
             return (DInt)
 
tipoChar = do symbol "Char"
              return (DChar)

tipoFloat = do symbol "Float"
               return (DFloat)

