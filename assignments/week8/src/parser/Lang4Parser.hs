module Lang4Parser where

import Data.Map (Map)-- for env
import qualified Data.Map as Map -- for env in tests

import Reader
import Lang4 (Ast(..), eval)

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
     (mults res) <|> return res

varExpr :: Parser Ast
varExpr = do s <- varParser
             return (Var s)

multExpr :: Parser Ast
multExpr =  do l <- parensOrInt
               mults l <|> return l

letExpr :: Parser Ast
letExpr = do token $ literal "let "
             v <- varParser
             token $ literal "="
             ast <- addSubMulExprOrPOI
             token $ literal "in"
             ast2 <- letExpr <|> addSubMulExprOrPOI
             return (Let v ast ast2)

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast


parensOrInt :: Parser Ast
parensOrInt = parens <|> parseAstInt <|> varExpr

multEprOrParensOrInt :: Parser Ast
multEprOrParensOrInt =  multExpr <|> parens <|> parseAstInt <|> varExpr

addSubMulExprOrPOI :: Parser Ast
addSubMulExprOrPOI = addSubExpr <|> multExpr <|> parens <|> parseAstInt <|> varExpr


parser :: Parser Ast
parser = letExpr <|> addSubExpr <|> multExpr <|> parens <|> parseAstInt <|> varExpr



-- for repl testing
data Lang4Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean env keeping only the result
exec :: String -> Lang4Out
exec s = case (parse parser) s of
  Just (ast,"") -> Result $ runReader (eval ast) Map.empty
  _  -> ParseError
