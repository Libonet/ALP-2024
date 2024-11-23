import Parsing
import Control.Applicative ((<|>))
import Data.Char


paren :: Parser [Int]
paren = do (xs, n) <- paren'
           if n /= 1 then failure else return xs

paren' :: Parser ([Int], Int)
paren' = do symbol "("
            (xs, n) <- paren'
            return (n-1:xs, n-1)
         <|>
         do symbol ")"
            (xs, n) <- paren'
            return (n:xs, n+1)
         <|>
         return ([], 1)
