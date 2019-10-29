module Lang4Parser where

import Data.Map (Map)-- for env
import qualified Data.Map as Map -- for env in tests

import Lang4 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang4
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")

literalIntParser :: Parser Ast
literalIntParser = token intParser
  `mapParser` (\ i -> LiteralInt i)

plusParser :: Parser Ast
plusParser = token (literal "(") +++ parser +++ token (literal "+") +++ parser +++ token (literal ")")
  `mapParser` (\ ((((_ , l),_),r),_) -> Plus l r)

letParser :: Parser Ast
letParser = token (literal "(let ") +++ varParser +++ token (literal "=") +++ parser +++ token (literal "in") +++ parser +++ token (literal ")")
  `mapParser` (\ ((((((_ , l),_),r),_),z),_) -> Let l r z)

parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
  `mapParser` (\ ((_,ast),_) -> ast)

myvarParser :: Parser Ast
myvarParser = (mapParser) varParser (\a -> Var a)


parser :: Parser Ast
parser = literalIntParser <||> plusParser <||> parseParens <||> letParser <||> myvarParser
  `mapParser` (\ e -> case e of
     -- Left (Left (Left (Left (Left ast)))) -> ast
     -- Left (Left (Left (Left (Right ast)))) -> ast
    Left (Left (Left (Left ast))) -> ast
    Left (Left (Left (Right ast))) -> ast
    Left (Left (Right ast)) -> ast
    Left (Right ast) -> ast
    Right ast -> ast)


-- for repl testing, will only work if you implement Lang3's eval
data Lang4Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean env keeping only the result
exec :: String -> Lang4Out
exec s = case parser s of
  Just (ast,"") -> case eval ast Map.empty of
    (Just i) -> Result i
    _  -> RuntimeError
  _  -> ParseError
