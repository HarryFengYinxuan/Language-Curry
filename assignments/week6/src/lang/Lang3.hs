module Lang3 where

import Data.Map (Map)-- for state
import qualified Data.Map as Map


data Ast =
      LiteralInt Integer
    | Var String
    | Plus Ast Ast
    | Assign String Ast
    | Separator Ast Ast


type State = Map String Integer

 -- ungraded bonus, helpful for testing
eval :: Ast -> State -> (Maybe Integer, State)
eval (LiteralInt i) s = (Just i, s)
eval (Var str) s = ((Map.lookup str s), s)
eval (Plus exp1 exp2) s = case (eval exp1 s) of
                         (Just i, s1) -> case (eval exp2 s1) of
                           (Just j, s2) -> (Just (i+j), s2)
                           (Nothing, s3) -> (Nothing, s3)
                         (Nothing, s4) -> (Nothing, s4)
eval (Assign str exp1) s = case (eval exp1 s) of
                             (Nothing, s1) -> (Nothing, s1)
                             (Just i, s2) -> (Just i, Map.insert str i s2)
eval (Separator exp1 exp2) s = let (_, s1) = eval exp1 s in case (eval exp2 s1) of
                                 (a, s2) -> (a, s2)


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (Var s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Assign s b) =  "(" ++ s ++ " := " ++ show b ++ ")"
