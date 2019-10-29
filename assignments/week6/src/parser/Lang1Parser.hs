module Lang1Parser where

import Lang1 (Ast(..), eval)
import MyParserLib

literalIntParser :: Parser Ast
literalIntParser = token intParser
  `mapParser` (\ i -> LiteralInt i)

plusParser :: Parser Ast
plusParser = token (literal "(") +++ parser +++ token (literal "+") +++ parser +++ token (literal ")")
  `mapParser` (\ ((((_ , l),_),r),_) -> Plus l r)

divParser :: Parser Ast
divParser = token (literal "(") +++ parser +++ token (literal "/") +++ parser +++ token (literal ")")
  `mapParser` (\ ((((_ , l),_),r),_) -> Div l r)

parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
  `mapParser` (\ ((_,ast),_) -> ast)


parser :: Parser Ast
parser = literalIntParser <||> plusParser <||> parseParens <||> divParser
  `mapParser` (\ e -> case e of
    Left (Left (Left ast)) -> ast
    Left (Left (Right ast)) -> ast
    Left (Right ast) -> ast
    Right ast -> ast)

-- parse the fully parenthesized Lang1
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")



-- for repl testing, will only work if you implement Lang1's eval
data Lang1Out = ParseError | DivZeroErro | Result Integer deriving (Show, Eq)

exec :: String -> Lang1Out
exec s = case parser s of
  Just (ast,"") -> case eval ast of
    Just i -> Result i
    _      -> DivZeroErro
  _  -> ParseError
