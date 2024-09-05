import Parsing
import Control.Applicative ((<|>))
import Data.Char

data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Add | Mul | Min | Div deriving Show

expr :: Parser Expr
expr = do space
          t <- term
          sumParse t <|> minParse t <|> return t

sumParse t = do symbol "+"
                t' <- expr
                return (BinOp Add t t')

minParse t = do symbol "-"
                t' <- expr
                return (BinOp Min t t')

term :: Parser Expr
term = do f <- factor
          mulParse f <|> divParse f <|> return f

mulParse f = do symbol "*"
                f' <- term
                return (BinOp Mul f f')

divParse f = do symbol "/"
                f' <- term
                return (BinOp Div f f')

factor :: Parser Expr
factor = do do d <- digit
               return (Num $ digitToInt d)
            <|> (do symbol "("
                    e <- expr
                    symbol ")"
                    return e)