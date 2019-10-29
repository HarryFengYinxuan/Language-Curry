module Lang4 where

-- we will use the standard Map
import Data.Map (Map)-- for Env
import qualified Data.Map as Map

-- We will now add let expressions to the abstract syntax tree.
-- Let String Ast Ast makes a local assignment of the first Ast to the String
-- in the state and evaluates the second Ast in this environment and returns
-- the result of the second Ast.
-- We will make a very simple extension to the example AST by adding a separator this will
-- join two abstract syntax trees; both will be evaluated, but only the value of the second
-- will be returned.

data Ast =
      LiteralInt Integer
    | Id String
    | Plus Ast Ast
    | Let String Ast Ast

type Env = Map String Integer

eval :: Ast -> Env -> Maybe Integer
eval (LiteralInt i) (e) = Just i
eval (Id s) e = Map.lookup s e
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
  show (Id s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Let s val inThis) =  "(let " ++ s ++ " = " ++ show val ++ " in " ++ show inThis ++ ")"
