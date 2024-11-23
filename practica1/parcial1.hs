import Parsing
import Control.Applicative ((<|>))
import Data.Char

-- lista = ([ IntList ] | Int : lista) lista'
-- lista' = e | ++ lista
-- IntList = Int | Int,IntList


lista :: Parser [Int]
lista = do l <- do symbol "["
                   l' <- intList
                   symbol "]"
                   return l'
                <|>
                do i <- integer
                   symbol ":"
                   l' <- lista
                   return (i:l')
           f <- lista'
           return (f l)


lista' :: Parser ([Int] -> [Int])
lista' = do symbol "++"
            r <- lista
            return (++ r)
         <|> return id

intList = sepBy1 integer (char ',')

