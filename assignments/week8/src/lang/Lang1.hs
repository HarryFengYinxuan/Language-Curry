module Lang1 where

import HelpShow


data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Div Ast Ast
  | Sub Ast Ast
  | Mult Ast Ast
  deriving Show

eval :: Ast -> Maybe Integer
eval (LiteralInt a) = Just a
eval (Plus a b) = do x <- eval(a)
                     y <- eval(b)
                     z <- return $ x+y
                     return z
eval (Div a b) = case ((eval b) == (Just 0)) of
                    (False) -> (do x <- eval(a)
                                   y <- eval(b)
                                   z <- return $ x `div` y
                                   return z)
                    True -> Nothing
eval (Sub a b) =  do x <- eval(a)
                     y <- eval(b)
                     z <- return $ x-y
                     return z
eval (Mult a b) = do x <- eval(a)
                     y <- eval(b)
                     z <- return $ x*y
                     return z

-- show the fully parenthesized syntax
showFullyParen :: Ast -> String
showFullyParen (LiteralInt i) = show i
showFullyParen (l `Plus` r)   = "(" ++ showFullyParen l ++ " + " ++  showFullyParen r ++ ")"
showFullyParen (l `Div` r)    = "(" ++ showFullyParen l ++ " / " ++ showFullyParen r ++ ")"
showFullyParen (l `Sub` r)    = "(" ++ showFullyParen l ++ " - " ++  showFullyParen r ++ ")"
showFullyParen (l `Mult` r)   = "(" ++ showFullyParen l ++ " * " ++  showFullyParen r ++ ")"


showPretty :: Ast -> Integer -> String
showPretty (LiteralInt i) _ = show i
showPretty (l `Div` r)    d = parenthesize d 1 ((showPretty l 1) ++ " / " ++  (showPretty r 0))
showPretty (l `Mult` r)   d = parenthesize d 1 ((showPretty l 1) ++ " * " ++  (showPretty r 0))
showPretty (l `Plus` r)   d = parenthesize d 3 ((showPretty l 3) ++ " + " ++  (showPretty r 2))
showPretty (l `Sub` r)    d = parenthesize d 3 ((showPretty l 3) ++ " - " ++  (showPretty r 2))

--instance Show Ast where
--  show e = showPretty e 100
