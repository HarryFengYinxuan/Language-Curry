module Lang3Parser where

import Data.Map (Map)-- for state
import qualified Data.Map as Map -- for State in tests

import State
import Lang3 (Ast(..), eval)
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

seps :: Ast -> Parser Ast
seps left =
   do s <- (token $ literal ";")
      exp <- addSubMulExprOrPOI
      let res =  left `Separator` exp
      (seps res) <|> return res

assExpr :: Parser Ast
assExpr =
   do s <- varParser
      (token $ literal ":=")
      ast <- addSubExpr
      return (Assign s ast)

varExpr :: Parser Ast
varExpr = do s <- varParser
             return (Var s)

multExpr :: Parser Ast
multExpr =  do l <- parensOrInt
               mults l <|> return l

sepExpr :: Parser Ast
sepExpr = do l2 <- addSubMulExprOrPOI
             seps l2 <|> return l2

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast


parensOrInt :: Parser Ast
parensOrInt = parens <|> parseAstInt <|> assExpr <|> varExpr

multEprOrParensOrInt :: Parser Ast
multEprOrParensOrInt =  multExpr <|> parens <|> parseAstInt <|> assExpr <|> varExpr

addSubMulExprOrPOI :: Parser Ast
addSubMulExprOrPOI = addSubExpr <|> multExpr <|> parens <|> parseAstInt <|> assExpr <|> varExpr


parser :: Parser Ast
parser = sepExpr <|> addSubExpr <|> multExpr <|> parens <|> parseAstInt <|> assExpr <|> varExpr


-- for repl testing
data Lang3Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean state keeping only the result
exec :: String -> Lang3Out
exec s = case (parse parser) s of
  Just (ast,"") -> case runState (eval ast) Map.empty of
    (i, _) -> Result i
  _  -> ParseError
