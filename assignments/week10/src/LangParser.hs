module LangParser where

import Lang
import ParserMonad
import EnvUnsafe

keywords = ["if","then","else", "let", "in", "true","false"]

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

varStr :: Parser String
varStr = do s <- token $ varParser
            if s `elem` keywords
            then failParse
            else return s

ints :: Parser Ast
ints = do r <- intParser
          return $ ValInt r

bools :: Parser Ast
bools = do a <- literal "true" <||> literal "false"
           case (a) of
             Left a -> return $ ValBool True
             Right a -> return $ ValBool False

nil :: Parser Ast
nil = do literal "[]"
         return Nil

appsFunc :: Ast -> Parser Ast
appsFunc left = do s <- spaces
                   exp <- allList
                   let res = left `App` exp
                   (appsFunc res) <|> return res

apps :: Parser Ast
-- apps = withInfix undefined [("",App)] -- the tokens eat up all the spaces so we split on the empty string
apps = do l <- allList
          appsFunc l <|> return l


allList :: Parser Ast
allList = conExpr <|> nil

allList1 :: Parser Ast
allList1 = conExpr1 <|> nil

cons :: Ast -> Parser Ast
cons left = do s <- token $ (literal ":")
               exp <- orExpr
               let res = left `Cons` exp
               (cons res) <|> return res

conExpr :: Parser Ast
conExpr = do l <- orExpr
             cons l <|> return l

cons1 :: Ast -> Parser Ast
cons1 left =  do s <- token $ (literal ":")
                 exp <- orExpr
                 let res = left `Cons` exp
                 (cons res) <|> return res

conExpr1 :: Parser Ast
conExpr1 = do l <- orExpr
              return l

-- *LangParser> parse cons "1 : 4: true"
-- Just (1 : 4 : true,"")

orFunc :: Ast -> Parser Ast
orFunc left = do s <- token $ (literal "||")
                 exp <- andExpr
                 let res = left `Or` exp
                 (orFunc res) <|> return res

orExpr :: Parser Ast
orExpr = do l <- andExpr
            orFunc l <|> return l

andFunc :: Ast -> Parser Ast
andFunc left =  do s <- token $ (literal "&&")
                   exp <- addSubExpr
                   let res = left `And` exp
                   (andFunc res) <|> return res

andExpr :: Parser Ast
andExpr =  do l <- addSubExpr
              andFunc l <|> return l

-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")

addSub :: Ast -> Parser Ast
addSub left = do s <- token $ (literal "+") <||> (literal "-")
                 exp <- multDivExpr
                 let res = case s of
                             Left _  -> left `Plus` exp
                             Right _ -> left `Minus` exp
                 (addSub res) <|> return res


-- "5-6" "7+6" "2*4+1*3" "2*4 + 1 + 2 + 3" ...
addSubExpr :: Parser Ast
addSubExpr = do l <- multDivExpr
                addSub l <|> return l

-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")

multDiv :: Ast -> Parser Ast
multDiv left =  do s <- token $ (literal "*") <||> (literal "/")
                   exp <- notOrAtom
                   let res = case s of
                               Left _  -> left `Mult` exp
                               Right _ -> left `Div` exp
                   (multDiv res) <|> return res


-- "5-6" "7+6" "2*4+1*3" "2*4 + 1 + 2 + 3" ...
multDivExpr :: Parser Ast
multDivExpr = do l <- notOrAtom
                 multDiv l <|> return l

notExpr :: Parser Ast
notExpr = do l <- token $ literal "!"
             a <- allList1
             return (Not a)

notOrAtom :: Parser Ast
notOrAtom = notExpr <|> atoms

atoms:: Parser Ast
atoms = ints <|> bools  <|>  nil <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars

-- *LangParser> parse atoms "111"
-- Just (111,"")
-- *LangParser> parse atoms "  true"
-- Just (true,"")

ifParser :: Parser Ast
ifParser = do token $ literal "if"
              a <- parser
              token $ literal "then"
              b <- parser
              token $ literal "else"
              c <- parser
              return $ If a b c

letParser :: Parser Ast
letParser =  do token $ literal "let"
                a <- varStr
                token $ literal "="
                b <- parser
                token $ literal "in"
                c <- parser
                return $ Let a b c

-- *LangParser> parse letParser "let x=3 in x+x"
-- Just (let x = 3 in x + x,"")


lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  a <- varStr
                  token $ literal "->"
                  b <- parser
                  return $ Lam a b

parens :: Parser Ast
parens = do token $ literal "("
            a <- parser
            token $ literal ")"
            return a

parser :: Parser Ast
parser = apps

-- note that the parser must respect all the precedence and associativity rules expressed in the prettyShow function.
-- that means
-- ! binds more tightly than
-- * / which binds more tightly than
-- + - which binds more tightly than
-- && which binds more tightly than
-- || which binds more tightly than
-- : which binds more tightly than
-- {the application} which binds weakest of all

-- + - * / && || {the application} are left associative
-- : is right associative

-- we are mostly following the questionable c precedence rules

-- ungraded bonus: add additional pretty syntax for lists: [1,2,3,4]



-- for repl testing
data LangOut = ParseError | RuntimeError String | Result Val deriving Show

exec :: String -> LangOut
exec s = case (parse parser) s of
  Just (ast,"") -> case run ast of
                     Ok v -> Result v
                     Error e -> RuntimeError e
  _  -> ParseError
