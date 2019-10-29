module Lang3 where

import Prelude hiding (lookup, insert) -- save us a good function name

import Data.Map (Map)-- for state
import qualified Data.Map as Map

import HelpShow

import State


-- We went over this functionality in some labs
setVar :: String -> Integer -> State EvalState ()
setVar v i =
  do s <- get
     put (Map.insert v i s)

-- same as
--State (\ s -> ((), Map.insert v i s))

getVar :: String -> State EvalState Integer
getVar v =
  do s <- get
     case (Map.lookup v s) of
       Just i  -> return i
       Nothing -> return 0  -- since for this problem we may return 0 if the var is not set

-- some examples
ex = Assign "x" (LiteralInt 2 `Plus` LiteralInt 2)
       `Separator` (Var "x" `Mult` LiteralInt 5 )

-- run your monad like this
ex' = runState (eval ex) Map.empty

data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Assign String Ast
    | Separator Ast Ast
    | Sub Ast Ast
    | Mult Ast Ast
  deriving Show

type EvalState = Map String Integer


-- for simplicity do not we will not separately encode failure at the type level,
-- you may return 0 for variable that are not defined

eval :: Ast -> State EvalState Integer
eval (LiteralInt i) = State $ \s -> (i, s)
eval (Var v) = getVar v
eval (Plus l r) = State $ \s -> let (State st) = (eval l) in
                                let (ln, s2) = (st s) in
                                let (State st2) = (eval r) in
                                let (rn, s3) = (st2 s2) in
                                (ln+rn, s3)
eval (Assign v ast) = State $ \s -> let (State st) = (eval ast) in
                                    let (n, s2) = (st s) in
                                    (n, Map.insert v n s2)
eval (Separator exp1 exp2) = State $ \s -> let (State st) = (eval exp1) in
                                           let (n1, s2) = (st s) in
                                           let (State st2) = (eval exp2) in
                                           let (n2, s3) = (st2 s2) in
                                           (n2, s3)
eval (Sub exp1 exp2) = State $ \s -> let (State st) = (eval exp1) in
                                     let (n1, s2) = (st s) in
                                     let (State st2) = (eval exp2) in
                                     let (n2, s3) = (st2 s2) in
                                     (n1-n2, s3)
eval (Mult exp1 exp2) = State $ \s -> let (State st) = (eval exp1) in
                                      let (n1, s2) = (st s) in
                                      let (State st2) = (eval exp2) in
                                      let (n2, s3) = (st2 s2) in
                                      (n1*n2, s3)


-- show the fully parenthesized syntax
showFullyParen :: Ast -> String
showFullyParen (LiteralInt i)    = show i
showFullyParen (Var s)           = s
showFullyParen (l `Plus` r)      = "(" ++ showFullyParen l ++ " + " ++  showFullyParen r ++ ")"
showFullyParen (l `Sub` r)       = "(" ++ showFullyParen l ++ " - " ++  showFullyParen r ++ ")"
showFullyParen (l `Mult` r)      = "(" ++ showFullyParen l ++ " * " ++  showFullyParen r ++ ")"
showFullyParen (l `Separator` r) = "(" ++ showFullyParen l ++ " ; " ++  showFullyParen r ++ ")"
showFullyParen (Assign v b)      = "(" ++ v ++ " := " ++ show b ++ ")"


showPretty :: Ast -> Integer -> String
showPretty (LiteralInt i)     _ = show i
showPretty (Var s)            _ = s
showPretty (l `Mult` r)       d = parenthesize d 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0))
showPretty (l `Plus` r)       d = parenthesize d 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
showPretty (l `Sub` r)        d = parenthesize d 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2))
showPretty (Assign v b)       d = parenthesize d 6 (v ++ " := " ++  (showPretty b 6) )
showPretty (l `Separator` r)  d = parenthesize d 8 ((showPretty l 8) ++ " ; " ++  (showPretty r 7) ) -- binds most weakly


--instance Show Ast where
--  show e = showPretty e 100
