import Parsing
import Control.Applicative ((<|>))
import Data.Char

data Elem = LI Int | LC Char deriving Show
type Lista = [Elem]

listas = do symbol "["
            r <- sepBy letterOrDigit (symbol ",")
            symbol "]"
            return r

letterOrDigit = do symbol "'"
                   c <- letter
                   symbol "'"
                   return (LC c)
                <|> do res <- digit
                       return (LI (digitToInt res))
