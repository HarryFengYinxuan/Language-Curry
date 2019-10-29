module Lang4 where

import Data.Map (Map)-- for Env
import qualified Data.Map as Map

import HelpShow

import Reader

data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Let String Ast Ast
    | Sub Ast Ast
    | Mult Ast Ast
  deriving Show

type Env = Map String Integer

-- for simplicity do not we will not separately encode failure at the type level,
-- you may return 0 for variable that are not defined

eval :: Ast -> Reader Env Integer
eval (LiteralInt n) = Reader $ \e -> n
eval (Var v) = Reader $ \e -> case (Map.lookup v e) of
                              Nothing -> 0
                              Just i -> i
eval (Plus exp1 exp2) = Reader $ \e -> let n1 = runReader (eval exp1) e in
                                       let n2 = runReader (eval exp2) e in
                                       n1+n2
eval (Let str exp1 exp2) = Reader $ \e -> let n1 = runReader (eval exp1) e in
                                          let n2 = runReader (eval exp2) (Map.insert str n1 e) in
                                          n2
eval (Sub exp1 exp2) = Reader $ \e -> let n1 = runReader (eval exp1) e in
                                      let n2 = runReader (eval exp2) e in
                                      n1-n2
eval (Mult exp1 exp2) =  Reader $ \e -> let n1 = runReader (eval exp1) e in
                                        let n2 = runReader (eval exp2) e in
                                        n1*n2

eval' :: Ast -> Env -> Integer -- functions have are already defined with the Reader Monad
eval' = undefined -- Ungraded practice problem




-- show the fully parenthesized syntax
showFullyParen :: Ast -> String
showFullyParen (LiteralInt i)     = show i
showFullyParen (Var s)            = s
showFullyParen (l `Plus` r)       = "(" ++ showFullyParen l ++ " + " ++  showFullyParen r ++ ")"
showFullyParen (l `Sub` r)        = "(" ++ showFullyParen l ++ " - " ++  showFullyParen r ++ ")"
showFullyParen (l `Mult` r)       = "(" ++ showFullyParen l ++ " * " ++  showFullyParen r ++ ")"
showFullyParen (Let s val inThis) =  "(let " ++ s ++ " = " ++ showFullyParen val ++ " in " ++ showFullyParen inThis ++ ")"


showPretty :: Ast -> Integer -> String
showPretty (LiteralInt i)     _ = show i
showPretty (Var s)            _ = s
showPretty (l `Mult` r)       d = parenthesize d 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0))
showPretty (l `Plus` r)       d = parenthesize d 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
showPretty (l `Sub` r)        d = parenthesize d 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2))
showPretty (Let s val inThis) d = parenthesize d 5  "let " ++ s ++ " = " ++ showPretty val 4 ++ " in " ++ showPretty inThis 5 -- binds most weakly


--instance Show Ast where
--  show e = showPretty 100
