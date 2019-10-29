module Lang1 where


data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Div Ast Ast

 -- ungraded bonus, helpful for testing
eval :: Ast -> Maybe Integer
eval (LiteralInt a) = Just a
eval (Plus a b) = case eval a of
                  Nothing -> Nothing
                  Just c -> case eval b of
                    Nothing -> Nothing
                    Just d -> Just (d+c)
eval (Div a b) = case eval b of
                  Nothing -> Nothing
                  Just 0 -> Nothing
                  Just c -> case eval a of
                    Nothing -> Nothing
                    Just d -> Just (d `div` c)

-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
