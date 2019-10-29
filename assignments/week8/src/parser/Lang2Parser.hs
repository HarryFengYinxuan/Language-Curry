module Lang2Parser where

import Lang2 (Ast(..), eval)
import PrinterMonad(runPrinterMonad)
import ParserMonad

parseAstInt :: Parser Ast
parseAstInt = token intParser
                `mapParser` (\ i -> LiteralInt i)




addSubEx = parse (addSub (LiteralInt 7)) "+ 3 + 4"
-- parse all the top level pluses and minuses
-- "-6" "+6" "+1*3" "+ 1 + 2 + 3"
addSub :: Ast -> Parser Ast
addSub left = do s <- token $ (literal "+") <||> (literal "-")
                 exp <- multEprOrParensOrInt <|> printExpr
                 let res = case s of
                             Left _  -> left `Plus` exp
                             Right _ -> left `Sub` exp
                 (addSub res) <|> return res


-- "5-6" "7+6" "2*4+1*3" "2*4 + 1 + 2 + 3" ...
addSubExpr :: Parser Ast
addSubExpr = do l <- multEprOrParensOrInt <|> printExpr
                addSub l <|> return l

-- parse all the top level multiplication
-- "*6" "*(1+3)" "* 1 * 2 * 3"
mults :: Ast -> Parser Ast
mults left =
  do s <- (token $ literal "*")
     exp <- parensOrInt <|> printExpr
     let res =  left `Mult` exp
     (mults res) <|> return res

seps :: Ast -> Parser Ast
seps left =
   do s <- (token $ literal ";")
      exp <- addSubMulExprOrPOI
      let res =  left `Separator` exp
      (seps res) <|> return res

prints :: Ast -> Parser Ast
prints left =
   do s <- (token $ literal "Print")
      exp <- parens
      let res =  Print exp
      (seps res) <|> (addSub res) <|> return res

printExpr :: Parser Ast
printExpr = do l <- (token $ literal "print") +++ parensOrInt
               let (_, exp) = l in
                let res = Print exp in
                  return res

multExpr :: Parser Ast
multExpr =  do l <- parensOrInt <|> printExpr
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
parensOrInt = parens <|> parseAstInt

multEprOrParensOrInt :: Parser Ast
multEprOrParensOrInt =  multExpr <|> parens <|> parseAstInt

addSubMulExprOrPOI :: Parser Ast
addSubMulExprOrPOI = addSubExpr <|> multExpr <|> parens <|> parseAstInt <|> printExpr


parser :: Parser Ast
parser = sepExpr <|> printExpr <|> addSubExpr <|> multExpr <|> parens <|> parseAstInt


-- for repl testing
data Lang2Out = ParseError | Result Integer [Integer] deriving (Show, Eq)

exec :: String -> Lang2Out
exec s = case (parse parser) s of
  Just (ast,"") -> case runPrinterMonad (eval ast) of
    (ls,i) -> Result i ls
  _  -> ParseError
