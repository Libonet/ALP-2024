import Parsing
import Control.Applicative ((<|>))
import Data.Char

data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Add | Mul | Min | Div deriving Show

expr :: Parser Expr
expr = do space
          t <- term
          f <- expr'
          return (f t)

expr' :: Parser (Expr -> Expr)
expr' = sumParse <|> minParse <|> 

sumParse = do symbol "+"
              t <- term
              g <- expr'
              return (\e -> g (BinOp Add e t))

minParse = do symbol "-"
              t <- term
              g <- expr'
              return (\e -> g (BinOp Min e t))

term :: Parser Expr
term = do t <- factor
          f <- term'
          return (f t)

term' = mulParse <|> divParse <|>

mulParse = do symbol "*"
              t <- factor
              g <- term'
              return (\e -> g (BinOp Mul e t))

divParse = do symbol "/"
              t <- factor
              g <- term'
              return (\e -> g (BinOp Div e t))

factor :: Parser Expr
factor = do do d <- digit
               return (Num $ digitToInt d)
            <|> (do symbol "("
                    e <- expr
                    symbol ")"
                    return e)