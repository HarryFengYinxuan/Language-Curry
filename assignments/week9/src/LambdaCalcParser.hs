module LambdaCalcParser where
import LambdaCalcImplementation
import ParserMonad

lamExpr :: Parser Term
lamExpr = do token $ literal "\\"
             var <- token varParser
             token $ literal "->"
             t <- token varExpr <|> appExpr <|> lamExpr
             return (Lam var t)

varExpr :: Parser Term
varExpr = do var <- varParser
             return (FreeVar var)

appExpr :: Parser Term
appExpr = do {- token $ literal "(" -}
             t1 <- varExpr <|> lamExpr <|> appExpr
             spaces
             t2 <- varExpr <|> lamExpr <|> appExpr
             {- token $ literal ")" -}
             return (App t1 t2)

parser :: Parser Term
parser = lamExpr <|> varExpr <|> appExpr


-- ungraded bonus: also parse wild cards like _ so you can write ( \ _ -> x)
-- ungraded bonus: parse numbers into their church encodings

-- for repl testing
data LambdaOut = ParseError | Result Term deriving (Show, Eq)

exec :: String -> LambdaOut
exec s = case (parse parser) s of
  Just (ast,"") -> Result $ eval ast
  _  -> ParseError
