module Parser where

import Ast
import ParserMonad
import EnvUnsafe
import qualified Data.Map as Map
import Data.Map (Map)

-- | parser for the language
keywords = ["if","then","else", "let", "in", "true","false", "print"]

-- sat2 :: (String -> Bool) -> Parser String
-- sat2 f = do 
--     c <- item 
-- comLit :: String -> Parser String 
-- comLit s = do 
--     r <- (comMultExpr <|> comLineExpr) <||> literal s
--     (comMultExpr <|> comLineExpr)
--     case r of 
--         Left _ -> comLit s 
--         Right r' -> return r'


-- comLineExpr :: Parser ()
-- comLineExpr = do
--     literal "--"
--     comLineExpr'


-- comLineExpr' :: Parser ()
-- comLineExpr' = do 
--     c <- item 
--     case c == '\n' of 
--         True -> return ()
--         False -> comLineExpr'
     

-- comMultExpr :: Parser ()
-- comMultExpr = do
--     literal "{-"
--     comMultExpr'


-- comMultExpr' :: Parser ()
-- comMultExpr' = do 
--     c <- item
--     case c == '-' of 
--         True -> do
--             c2 <- item 
--             case c2 == '}' of 
--                 True -> return ()
--                 False -> comMultExpr'
--         False -> comMultExpr'
strHelper :: Parser String 
strHelper = do
    c <- item <||> return ""
    case c of 
        Left '\"' -> return ""
        Left c -> do 
            s <- strHelper
            return (c:s)
        Right _ -> return ""

comments :: Parser String 
comments = do
    c <- item <||> return ""
    case c of 
        Left c -> case (c=='{', c=='-') of 
            (False, False) -> do 
                s <- comments
                return $ c:s
            (True, _) -> do 
                c2 <- item <||> (return $ c:"")
                case c2 of 
                    Left c2 -> case c2 == '-' of 
                        True -> comMultPar'
                        False -> do 
                            s <- comments
                            return (c:c2:s)
                    _ -> return ""
            (_, True) -> do 
                c2 <- item <||> (return $ c:"")
                case c2 of 
                    Left c2 -> case c2 == '-' of 
                        True -> comLinePar'
                        False -> do 
                            s <- comments
                            return (c:c2:s)
                    _ -> return ""
        Right _ -> return ""


comMultPar :: Parser String 
comMultPar = do
    c <- item <||> return ""
    case c of 
        Left c -> case c == '{' of 
            True -> do
                c2 <- item 
                case c2 == '-' of 
                    True -> comMultPar'
                    False -> do 
                        s <- comMultPar
                        return (c:c2:s) <|> return (c:c2:"")
            False -> do 
                s <- comMultPar
                return (c:s) <|> return (c:"")
        Right _ -> return ""
    

comMultPar' :: Parser String 
comMultPar' = do
    c <- item <||> return ""
    case c of 
        Left c -> case c == '-' of 
            True -> do
                c2 <- item 
                case c2 == '}' of 
                    True -> comments
                    False -> do
                        s <- comMultPar'
                        return (s)
            False -> do
                s <- comMultPar'
                return (s)
        Right _ -> return ""
    
comLinePar :: Parser String 
comLinePar = do
    c <- item <||> return ""
    case c of 
        Left c -> case c == '-' of 
            True -> do
                c2 <- item 
                case c2 == '-' of 
                    True -> comLinePar'
                    False -> do 
                        s <- comLinePar
                        return (c:c2:s) <|> return (c:c2:"")
            False -> do 
                s <- comLinePar
                return (c:s) <|> return (c:"")
        Right _ -> return ""
    

comLinePar' :: Parser String 
comLinePar' = do
    c <- item <||> return ""
    case c of 
        Left c -> case c == '\n' of 
            True -> comments
            False -> comLinePar'
        Right _ -> return ""


typeExpr :: Parser Type 
typeExpr = do token $ literal ":"
              s <- token $ ((literal "i") <|> (literal "f") <|> (literal "b") <|> (literal "c") <|> (literal "s") <|> (literal "l") <|> (literal "v") <|> (literal "x"))
              return (typeTable Map.! s)

infixr 5 `cons`
cons = Cons

vars :: Parser Ast
vars = do s <- token $ varParser
          t <- typeExpr
          if s `elem` keywords
          then failParse
          else return $ Var s t

varStr :: Parser String
varStr = do s <- token $ varParser
            if s `elem` keywords
            then failParse
            else return s

ints :: Parser Ast
ints = do r <- intParser
          return $ ValInt r

floats :: Parser Ast 
floats = do i <- intParser
            token $ literal "."
            i2 <- intParser
            return (ValFloat (read ((show i) ++ "." ++ (show i2))))

chars :: Parser Ast
chars = do token $ literal "'"
           i <- item
           token $ literal "'"
           return $ ValChar i

strs :: Parser Ast
strs =   do token $ literal "\""
            s <- strHelper
            -- token $ literal "\""
            return $ ValStr s

bools :: Parser Ast
bools = do a <- literal "true" <||> literal "false"
           case (a) of
             Left a -> return $ ValBool True
             Right a -> return $ ValBool False

nil :: Parser Ast
nil = do literal "[]"
         return $ Nil

sepsFunc :: Ast -> Parser Ast 
sepsFunc left = do s <- token $ (literal ";") 
                   exp <- apps 
                   let res = left `Sep` exp 
                   (sepsFunc res) <|> return res 

seps :: Parser Ast 
seps = do l <- apps 
          sepsFunc l <|> return l

appsFunc :: Ast -> Parser Ast
appsFunc left =  do s <- spaces
                    exp <- orExpr
                    let res = left `App` exp
                    (appsFunc res) <|> return res

apps :: Parser Ast
-- apps = withInfix undefined [("",App)] -- the tokens eat up all the spaces so we split on the empty string
apps = do l <- orExpr
          appsFunc l <|> return l

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
                   exp <- compLevel
                   let res = left `And` exp
                   (andFunc res) <|> return res

andExpr :: Parser Ast
andExpr =  do l <- compLevel
              andFunc l <|> return l

-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")

eqExpr :: Parser Ast 
eqExpr = do l <- concatExpr
            s <- token $ (literal "==") <||> (literal "/=")
            r <- concatExpr
            let res = case s of 
                        (Left _) -> l `Ast.EQ` r 
                        (Right _) -> l `Ast.NEQ` r
            return res

lteExpr :: Parser Ast 
lteExpr =    do l <- concatExpr
                s <- token $ (literal "<=") <||> (literal "<")
                r <- concatExpr
                let res = case s of 
                            (Left _) -> l `Ast.LTE` r 
                            (Right _) -> l `Ast.LT` r
                return res 

gteExpr :: Parser Ast 
gteExpr =    do l <- concatExpr
                s <- token $ (literal ">=") <||> (literal ">")
                r <- concatExpr
                let res = case s of 
                            (Left _) -> l `Ast.GTE` r 
                            (Right _) -> l `Ast.GT` r
                return res 

compLevel :: Parser Ast 
compLevel = eqExpr <|> lteExpr <|> gteExpr <|> concatExpr

allList :: Parser Ast
allList =  nil <|> addSubExpr <|> conExpr

-- conFunc :: Ast -> Parser Ast 
-- conFunc left = do token $ literal ","
--                   t <- addSubExpr
--                   let res = left `cons` t
--                   conFunc res <|> return res

addNil :: Ast -> Ast 
addNil (Cons a Nil) = (Cons a Nil)
addNil (Cons a b) = Cons a (addNil b)
addNil (a) = Cons a Nil

concatFunc :: Ast -> Parser Ast 
concatFunc l = do s <- token $ (literal "++")
                  c <- allList
                  let res = l `Concat` c
                  concatFunc res <|> return res 

concatExpr :: Parser Ast 
concatExpr = do l <- allList
                concatFunc l <|> return l

conExpr :: Parser Ast
conExpr =  do token $ literal "["
              c <- conExpr'
              token $ literal "]"
              return (addNil c)

-- conExpr' :: Ast -> Parser Ast
-- conExpr' l = do token $ literal ","
--                 t <- conExpr' l <|> addSubExpr
--                 return (Cons h t) <|> return h

append :: Ast -> Ast -> Ast
append (Nil) a = Cons a Nil 
append (Cons a Nil) b = Cons a (Cons b Nil)
append (Cons a b) c = Cons a (append b c)
append a b = Cons a (Cons b Nil)

conExpr' :: Parser Ast 
conExpr' = do l <- addSubExpr
              conFunc (Cons l Nil) <|> return l

conFunc :: Ast -> Parser Ast 
conFunc l = do token $ literal ","
               exp <- addSubExpr
               let res = (l `append` exp)
               (conFunc res) <|> (return res)

addSub :: Ast -> Parser Ast
addSub left = do s <- token $ (literal "+") <||> (literal "-")
                 exp <- mdmLevel
                 let res = case s of
                             Left _  -> left `Plus` exp
                             Right _ -> left `Minus` exp
                 (addSub res) <|> return res


-- "5-6" "7+6" "2*4+1*3" "2*4 + 1 + 2 + 3" ...
addSubExpr :: Parser Ast
addSubExpr = do l <- mdmLevel
                addSub l <|> return l

-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")

multDiv :: Ast -> Parser Ast
multDiv left =  do s <- token $ (literal "*") <||> ((literal "//") <||> (literal "/"))
                   exp <- expExpr
                   let res = case s of
                               Left _  -> left `Mult` exp
                               Right (Left _) -> left `DivI` exp
                               Right (Right _) -> left `DivF` exp
                   (multDiv res) <|> (modFunc res) <|> return res

-- "5-6" "7+6" "2*4+1*3" "2*4 + 1 + 2 + 3" ...
multDivExpr :: Parser Ast
multDivExpr = do l <- expExpr
                 multDiv l <|> return l

modFunc :: Ast -> Parser Ast 
modFunc left = do token $ literal "%"
                  exp <- expExpr
                  let res = left `Ast.Mod` exp
                  (modFunc res) <|> (multDiv res) <|> return res
                   
modExpr :: Parser Ast 
modExpr = do l <- expExpr
             modFunc l

mdmLevel :: Parser Ast 
mdmLevel = modExpr <|> multDivExpr <|> expExpr

expFunc :: Ast -> Parser Ast 
expFunc left =   do s <- token $ (literal "^") <||> (literal "**")
                    t <- expExpr <|> indLevel
                    let res = case s of 
                                Left _ -> left `ExpF` t
                                Right _ -> left `ExpI` t
                    return res <|> return res


expExpr :: Parser Ast
expExpr = do left <- indLevel
             (expFunc left) <|> return left

indexExpr :: Parser Ast 
indexExpr = do l <- conExpr
               token $ literal "!!"
               i <- ints
               return $ Ind (l) (i)

indLevel :: Parser Ast 
indLevel = indexExpr <|> nupLevel

notExpr :: Parser Ast
notExpr = do l <- token $ literal "!"
             a <- atoms <|> notExpr
             return (Not a)

notOrAtom :: Parser Ast
notOrAtom = notExpr <|> atoms

uminusExpr :: Parser Ast 
uminusExpr = do token $ literal "-"
                a <- atoms <|> uminusExpr
                return (UMinus a)

printExpr :: Parser Ast 
printExpr = do token $ literal "print("
               r <- parser 
               token $ literal ")"
               return (Print r)

nupLevel :: Parser Ast 
nupLevel = printExpr <|> uminusExpr <|> notExpr <|> atoms

atoms:: Parser Ast
atoms = (floats <|> ints <|> bools <|> chars <|> strs <|> nil <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars)
    

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
                  t <- typeExpr
                  token $ literal "->"
                  b <- parser
                  return $ Lam a t b

parens :: Parser Ast
parens = do token $ literal "("
            a <- parser
            token $ literal ")"
            return a

parser :: Parser Ast
parser = seps


myPar :: String -> (Maybe (Ast, String))
myPar s = case parse comments s of 
    Just (s, "") -> parse parser s
    Just (s, _) -> (Nothing)
    (Nothing) -> Nothing


-- note that the parser must respect all the precedence and associativity rules expressed in the prettyShow function.
-- that means
-- atoms
-- !, uminus, print binds more tightly than
-- list index
-- float and int exp R associative
-- * / // mod which binds more tightly than
-- + - which binds more tightly than
-- : ++ R associative
-- eq and stuff
-- && which binds more tightly than
-- || which binds more tightly than
-- {the application} which binds weakest of all
-- sep R associative

-- + - * / && || {the application} are left associative
-- : is right associative

-- we are mostly following the questionable c precedence rules

-- ungraded bonus: add additional pretty syntax for lists: [1,2,3,4]


