module Lang4 where

import Data.Map (Map)-- for Env
import qualified Data.Map as Map


data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Let String Ast Ast

type Env = Map String Integer

 -- ungraded bonus, helpful for testing

eval :: Ast -> Env -> Maybe Integer
eval (LiteralInt i) (e) = Just i
eval (Var s) e = Map.lookup s e
eval (Plus a1 a2) e = case (eval a1 e) of
                   Nothing -> Nothing
                   Just i1 -> case (eval a2 e) of
                     Nothing -> Nothing
                     Just i2 -> Just (i1+i2)
eval (Let str exp1 exp2) e = case (eval exp1 e) of
                               Nothing -> case (eval exp2 (e)) of
                                 Nothing -> Nothing
                                 Just i -> Just i
                               Just i -> case (eval exp2 (Map.insert str i e)) of
                                 Nothing -> Nothing
                                 Just j -> Just j


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (Var s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Let s val inThis) =  "(let " ++ s ++ " = " ++ show val ++ " in " ++ show inThis ++ ")"
