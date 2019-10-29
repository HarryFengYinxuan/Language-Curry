module Lang3Parser where

import Data.Map (Map)-- for state
import qualified Data.Map as Map -- for State in tests

import Lang3 (Ast(..), eval)
import MyParserLib


-- parse the fully parenthesized Lang3
-- ungraded bonus, handle some expressions without parentheses (like: "1+2+3" instead of "(1+(2+3))")

literalIntParser :: Parser Ast
literalIntParser = token intParser
  `mapParser` (\ i -> LiteralInt i)

plusParser :: Parser Ast
plusParser = token (literal "(") +++ parser +++ token (literal "+") +++ parser +++ token (literal ")")
  `mapParser` (\ ((((_ , l),_),r),_) -> Plus l r)

sepParser :: Parser Ast
sepParser = token (literal "(") +++ parser +++ token (literal ";") +++ parser +++ token (literal ")")
  `mapParser` (\ ((((_ , l),_),r),_) -> Separator l r)

assParser :: Parser Ast
assParser = token (literal "(") +++ varParser +++ token (literal ":=") +++ parser +++ token (literal ")")
  `mapParser` (\ ((((_ , l),_),r),_) -> Assign l r)

parseParens :: Parser Ast
parseParens = token (literal "(") +++ parser +++ token (literal ")")
  `mapParser` (\ ((_,ast),_) -> ast)

myvarParser :: Parser Ast
myvarParser = (mapParser) varParser (\a -> Var a)


parser :: Parser Ast
parser = literalIntParser <||> plusParser <||> parseParens <||> sepParser <||> assParser <||> myvarParser
  `mapParser` (\ e -> case e of
    Left (Left (Left (Left (Left ast)))) -> ast
    Left (Left (Left (Left (Right ast)))) -> ast
    Left (Left (Left (Right ast))) -> ast
    Left (Left (Right ast)) -> ast
    Left (Right ast) -> ast
    Right ast -> ast)

-- for repl testing, will only work if you implement Lang3's eval
data Lang3Out = ParseError | RuntimeError | Result Integer deriving (Show, Eq)

-- execute in a clean state keeping only the result
exec :: String -> Lang3Out
exec s = case parser s of
  Just (ast,"") -> case eval ast Map.empty of
    (Just i, _) -> Result i
    _  -> RuntimeError
  _  -> ParseError
