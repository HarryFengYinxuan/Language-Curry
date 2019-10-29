module Lang2 where

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Separator Ast Ast
  | Print Ast

 -- ungraded bonus, helpful for testing
eval :: Ast -> ([Integer], Integer)
eval (LiteralInt i) = ([], i)
eval (Plus a b) = case (eval a) of
                 (printList, ret)-> case (eval b) of
                   (printList2, ret2) -> (printList++printList2, ret+ret2)
eval (Print exp) = case (eval exp) of
                   (printList, ret) -> (printList++[ret], ret)
eval (Separator exp1 exp2) =  case (eval exp1) of
                               (printList, ret) -> case (eval exp2) of
                                 (printList2, ret2) -> (printList++printList2, ret2)


-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Print b) =  "print(" ++ show b ++ ")"
