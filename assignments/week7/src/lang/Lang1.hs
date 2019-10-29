module Lang1 where


data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Div Ast Ast
  | Sub Ast Ast
  | Mult Ast Ast

-- hint: use do notation from the built in monad on Maybe
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
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
  show (l `Sub` r) = "(" ++ (show l) ++ " - " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
