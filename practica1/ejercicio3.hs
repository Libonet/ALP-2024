import Parsing
import Control.Applicative ((<|>))
import Data.Char

trans parser =  do symbol "("
                   res <- parser
                   symbol ")"
                   return res
                <|> parser   