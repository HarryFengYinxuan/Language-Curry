module Lang1Parser where

import Lang1 (Ast(..), eval)
import ParserMonad

parseAstInt :: Parser Ast
parseAstInt = token intParser
                `mapParser` (\ i -> LiteralInt i)



addSubEx = parse (addSub (LiteralInt 7)) "+ 3 + 4"
-- parse all the top level pluses and minuses
-- "-6" "+6" "+1*3" "+ 1 + 2 + 3"
addSub :: Ast -> Parser Ast
addSub left = do s <- token $ (literal "+") <||> (literal "-")
                 exp <- multEprOrParensOrInt
                 let res = case s of
                             Left _  -> left `Plus` exp
                             Right _ -> left `Sub` exp
                 (addSub res) <|> return res


-- "5-6" "7+6" "2*4+1*3" "2*4 + 1 + 2 + 3" ...
addSubExpr :: Parser Ast
addSubExpr = do l <- multEprOrParensOrInt
                addSub l <|> return l

-- parse all the top level multiplication
-- "*6" "*(1+3)" "* 1 * 2 * 3"
mults :: Ast -> Parser Ast
mults left =
  do s <- (token $ literal "*")
     exp <- parensOrInt
     let res =  left `Mult` exp
     (divs res) <|> (mults res) <|> return res

divs :: Ast -> Parser Ast
divs left =
   do s <- (token $ literal "/")
      exp <- parensOrInt
      let res =  left `Div` exp
      (mults res) <|> (divs res) <|> return res


multEpr :: Parser Ast
multEpr = do l <- parensOrInt
             mults l

divsEpr :: Parser Ast
divsEpr = do l <- parensOrInt
             divs l

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast


parensOrInt :: Parser Ast
parensOrInt = parens <|> parseAstInt

multEprOrParensOrInt :: Parser Ast
multEprOrParensOrInt =  multEpr <|> divsEpr <|> parens <|> parseAstInt


parser :: Parser Ast
parser = addSubExpr <|> multEpr <|> divsEpr <|> parens <|> parseAstInt


-- for repl testing
data Lang1Out = ParseError | DivZeroErro | Result Integer deriving (Show, Eq)

exec :: String -> Lang1Out
exec s = case (parse parser) s of
  Just (ast,"") -> case eval ast of
    Just i -> Result i
    _      -> DivZeroErro
  _  -> ParseError
