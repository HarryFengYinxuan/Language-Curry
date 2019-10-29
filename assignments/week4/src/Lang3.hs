module Lang3 where

-- we will use the standard Map
import Data.Map (Map)-- for state
import qualified Data.Map as Map

-- We will now add identifiers and a global assignment command to the abstract syntax tree.
-- Assignment should evaluate to the value of the assignment and store the value in the global memory state.
-- The state (containing values for variables) is passed along as the evaluation proceeds; as Assign
-- expressions are evaluated, bindings are added to the state, and when Id expressions are evaluated
-- they are looked up in the state. Imagine walking around the AST in preorder and keeping track
-- of the state as we do so.

-- We will make a very simple extension to the example AST by adding a separator this will
-- join two abstract syntax trees; both will be evaluated, but only the value of the second
-- will be returned.

data Ast =
      LiteralInt Integer
    | Id String
    | Plus Ast Ast
    | Assign String Ast
    | Separator Ast Ast


type State = Map String Integer

-- hint use lookup

eval :: Ast -> State -> (Maybe Integer, State)
eval (LiteralInt i) s = (Just i, s)
eval (Id str) s = ((Map.lookup str s), s)
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
  show (Id s) = s
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Assign s b) =  "(" ++ s ++ " := " ++ show b ++ ")"
