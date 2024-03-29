module Lang where

import Data.Map (Map)
import qualified Data.Map as Map

import HelpShow

import EnvUnsafe


-- Here is the abstract syntax tree for our language

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast

         | Nil
         | Cons Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
--           deriving (Eq,Show) -- helpful to use this during testing
         deriving Eq

instance Show Ast where
  show ast = showPretty ast 0


-- the goal of the program is to return a value
data Val = I Integer | B Bool
         | Ls [Val]
         | Fun (Val -> Unsafe Val) -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function


stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> Ok $ Ls ls
                                   _         -> Error "can only call tail on a non empty list"),
   ("head", Fun $ \ v -> case v of
                         Ls [] -> Error "can only call head on a non empty list"
                         Ls list -> Ok $ head list
                         _ -> Error "not a list"),
   ("len", Fun $ \ v -> case v of
                        Ls list -> Ok $ I $ toInteger $ length list
                        _ -> Error "not a list")]

-- helper function that runs with a standard library of functions: head, tail ...
run :: Ast -> Unsafe Val
run a = runEnvUnsafe (eval a) stdLib



type Env = Map String Val

evalBool :: Ast -> EnvUnsafe Env Bool
evalBool a = do x <- eval a
                case x of
                  B b -> return b
                  _ -> err "not a bool"

evalInt :: Ast -> EnvUnsafe Env Integer
evalInt a =  do x <- eval a
                case x of
                  I b -> return b
                  _ -> err "not a int"

evalList :: Ast -> EnvUnsafe Env [Val]
evalList (Cons a b) = do ar <- eval a
                         br <- evalList b
                         return (ar:br)
evalList (Nil) = return []
evalList _ =     err "not a list"

evalFun :: Ast -> EnvUnsafe Env (Val -> Unsafe Val)
evalFun a =  do x <- eval a
                case x of
                  Fun b -> return b
                  _ -> err "not a func"

eval :: Ast -> EnvUnsafe Env Val
eval (ValBool b) = return $ B b
eval (And a b) =  do ar <- evalBool a
                     br <- evalBool b
                     return $ B $ ar && br
eval (Or a b) =  do ar <- evalBool a
                    br <- evalBool b
                    return $ B $ ar || br
eval (Not a) = do ar <- evalBool a
                  return $ B $ not ar

eval (ValInt i) = return $ I i
eval (Plus a b) =  do ar <- evalInt a
                      br <- evalInt b
                      return $ I $ ar + br
eval (Minus a b) =  do ar <- evalInt a
                       br <- evalInt b
                       return $ I $ ar - br
eval (Mult a b) =  do ar <- evalInt a
                      br <- evalInt b
                      return $ I $ ar * br
eval (Div a b) =  do ar <- evalInt a
                     br <- evalInt b
                     case (br==0) of
                      True -> err "can't divided by zero"
                      False -> return $ I $ ar `div` br
eval (Nil) = return $ Ls []
eval (Cons a b) = do list <- evalList (Cons a b)
                     return $ Ls $ list
eval (If a b c) = do ar <- evalBool a
                     br <- eval b
                     cr <- eval c
                     case (ar) of
                      True -> eval b
                      False -> eval c
eval (Let s a b) = do ar <- eval a
                      env <- getEnv
                      case runEnvUnsafe (eval b) (Map.insert s ar env) of
                        Ok a -> return a
                        Error e -> err e
eval (Var v) = do env <- getEnv
                  case (Map.lookup v env) of
                    Just a -> return a
                    Nothing -> err "No such var"
eval (Lam s a) = do env <- getEnv
                    return $ Fun $ \ v -> runEnvUnsafe (eval a) $ Map.insert s v env
eval (App a b) = do f <- evalFun a
                    br <- eval b
                    case (f br) of
                      Ok r -> return r
                      Error e -> err e

-- This is helpful for testing and debugging
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"


-- provide a nice show with minimal parentheses, for testing an documentation



--the bigger the number the more tight the biding
showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s

showPretty (Lam v bod) i = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 1)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

showPretty (App l r) i = parenthesize 2 i $ (showPretty l 2) ++ " " ++ (showPretty r 3)
showPretty (Cons l r) i = parenthesize 4 i $ (showPretty l 5) ++ " : " ++ (showPretty r 4)
showPretty (Or l r) i = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)
showPretty (And l r) i = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)
showPretty (Minus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " - " ++ (showPretty r 11)
showPretty (Plus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " + " ++ (showPretty r 11)
showPretty (Mult l r) i = parenthesize 12 i $ (showPretty l 12) ++ " * " ++ (showPretty r 13)
showPretty (Div l r) i = parenthesize 12 i $ (showPretty l 12) ++ " / " ++ (showPretty r 13)

showPretty (Not l ) i = parenthesize 14 i $  " ! " ++ (showPretty l 14)
