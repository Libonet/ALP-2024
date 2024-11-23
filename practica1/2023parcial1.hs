import Parsing
import Control.Applicative ((<|>))
import Data.Char

-------------------
{-
Ejercicio 2

max: dados 2 naturales, devuelve el mayor de ellos

menos a b = b pred a => menos = \a b . b pred a

max = \a b . if isZero (menos a b) then b else a = \a b . isZero (menos a b) b a


maximum: dada una lista de naturales, devuelve el mayor (no usar operador de punto fijo)

maximum = \xs . foldr xs (max) 0 = \xs . xs max 0


mod: dados 2 naturales, devuelve el resto de la división. Usar operador de punto fijo Y

Y = \x . (\y . x (y y)) (\y . x (y y))

B = \f . \n m . if isZero (menos n m) then n else f (menos n m) m = \f . \n m . isZero (menos n m) n (f (menos n m) m)

mod = Y B 

-}
-------------------


data GenTree a = N a [GenTree a] deriving Show

tree :: Parser (GenTree Int)
tree = do symbol "("
          symbol "N"
          v <- natural
          hijos <- many tree
          symbol ")"
          return (N v hijos)

ejemplo = "(N 2 (N 1 (N 5) (N 3)) (N 9) (N 4 (N 7)))"
