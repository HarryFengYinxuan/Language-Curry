module Eval where

import EnvUnsafe
import Data.Map (Map)
import qualified Data.Map as Map
import HelpShow
import Data.Char (ord, chr)

import Ast


data Val = I Integer | B Bool | F Float | S String | C Char
  | Ls [Val]
  | Fun (Val -> Unsafe Val) -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show (I i) = show i
  show (B b) = case b of 
    True -> "true"
    False -> "false"
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function
  show (F f) = show f 
  show (S s) = s 
  show (C c) = show c

instance Eq Val where 
  Fun _ == _ = False 
  _ == Fun _ = False 
  I i == I i2 = i == i2 
  B b == B b2 = b == b2 
  (F f) == (F f2) = f == f2 
  (S s) == (S s2) = s == s2 
  (C c) == (C c2) = c == c2 
  (Ls l) == (Ls l2) = l == l2
  _ == _ = False

-- helper function that runs with a standard library of functions: head, tail ...
run :: Ast -> (Unsafe Val, [String])
run a = runEnvUnsafeLog (eval a) stdLib

runJV :: Ast -> Unsafe Val
runJV a = let (uv, s) = runEnvUnsafeLog (eval a) stdLib in uv

stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of 
                         Ls (_:ls) -> Ok $ Ls ls
                         _         -> Error "can only call tail on a non empty list"),
  ("head", Fun $ \ v -> case v of
                        Ls [] -> Error "can only call head on a non empty list"
                        Ls list -> Ok $ head list
                        _ -> Error "not a list"),
  ("len",  Fun $ \ v -> case v of
                        Ls list -> Ok $ I $ toInteger $ length list
                        _ -> Error "not a list"),
  ("elem", Fun $ \i-> case i of 
    (i) -> Ok $ Fun $ \l -> case l of 
      Ls [] -> Ok $ B $ False
      Ls [a] -> Ok $ B $ a == i
      Ls (x:xs) -> case (x == i) of 
        True -> Ok $ B $ True 
        False -> (run2StdLib "elem" i (Ls xs))
      _ -> Error "Second arg of elem is not a list"),
  ("map", Fun $ \f -> case f of 
    (Fun f) -> Ok $ Fun $ \l -> case l of 
      (Ls []) -> Ok $ Ls []
      (Ls [a]) -> case f a of 
        Ok h -> Ok $ Ls [h] 
        Error e -> Error e 
      (Ls (a:as)) -> case (run2StdLib "map" (Fun f) (Ls as)) of 
        Ok (Ls as) -> case f a of 
          Ok h -> Ok $ Ls ((h): as)
          Error e -> Error e
        _ -> Error "Error with map"
      _ -> Error "Second arg of map must be a list"
    _ -> Error "First arg of map must be function"),
  ("filter", Fun $ \f -> case f of
    Fun f -> Ok $ Fun $ \l -> case l of 
      Ls [] -> Ok (Ls [])
      Ls [h] -> case f h of 
        Ok (B True) -> Ok $ Ls [h]
        Ok (B False) -> Ok $ Ls []
        Ok (_) -> Error "First argument of filter must give bool values"
        Error e -> Error e 
      Ls (h:t) -> case run2StdLib "filter" (Fun f) (Ls [h]) of 
        Ok (Ls l2) -> case run2StdLib "filter" (Fun f) (Ls t) of 
          Ok (Ls l3) -> Ok (Ls (l2 ++ l3))
          Ok _ -> Error "Error with filter"
          e -> e 
        Ok _ -> Error "Error with filter"
        e -> e 
      _ -> Error "Second arg of filter must be list"
    _ -> Error "First arg of filter must be function"),
  ("ord", Fun $ \c -> case c of 
    (C c) -> Ok (I (toInteger $ ord c))
    _ -> Error "Ord takes char"),
  ("chr", Fun $ \i -> case i of 
    (I i) -> Ok (C (chr $ fromInteger i))
    _ -> Error "Chr takes integer"),
  ("float", Fun $ \i -> case i of 
    (I i) -> Ok (F ((fromInteger i)::Float))
    _ -> Error "Float takes integer"),
  ("int", Fun $ \f -> case f of 
    (F f) -> Ok (I ((round f)::Integer))
    _ -> Error "Int takes float")]

runStdLib :: String -> Val -> Unsafe Val 
runStdLib s v = case (stdLib Map.! s) of 
  (Fun f) -> f v 
  _ -> Error "invalid stdlib function"

run2StdLib :: String -> Val -> Val -> Unsafe Val -- 2 args
run2StdLib s v1 v2 = case runStdLib s v1 of 
  Ok (Fun f) -> f v2 
  _ -> Error "bad run 2 arguments"


type Env = Map String Val

evalBool :: Ast -> EnvUnsafeLog Env Bool
evalBool a = do x <- eval a
                case x of
                  B b -> return b
                  _ -> err "not a bool"

evalInt :: Ast -> EnvUnsafeLog Env Integer
evalInt a =  do x <- eval a
                case x of
                  I b -> return b
                  _ -> err "not a int"
                  
evalFloat :: Ast -> EnvUnsafeLog Env Float 
evalFloat a = do x <- eval a 
                 case x of 
                    F f -> return f 
                    _ -> err "not a float"

evalList :: Ast -> EnvUnsafeLog Env [Val]
evalList (Cons a b) =  do ar <- eval a
                          br <- evalList b
                          return (ar:br)
evalList (Nil) = return []
evalList _ =     err "not a list"

evalFun :: Ast -> EnvUnsafeLog Env (Val -> Unsafe Val)
evalFun a =  do x <- eval a
                case x of
                  Fun b -> return b
                  _ -> err "not a func"

printFunc :: Val -> [String] ->  (EnvUnsafeLog Env Val)
printFunc v = \log -> EnvUnsafeLog $ \env -> (Ok v, ((show v):log))

eval :: Ast -> EnvUnsafeLog Env Val
eval (ValFloat v) = return $ F v
eval (ValChar v) = return $ C v
eval (ValStr v) = return $ S v
eval (ValBool b) = return $ B b
eval (And a b) = do ar <- evalBool a
                    br <- evalBool b
                    return $ B $ ar && br
eval (Or a b) =  do ar <- evalBool a
                    br <- evalBool b
                    return $ B $ ar || br
eval (Not a) = do ar <- evalBool a
                  return $ B $ not ar

eval (ValInt i) = return $ I i

-- probably a hardcore way to check type and reserve log
-- eval (Plus a b) = case (runJV a) of 
--                     (Ok (I ar)) -> case (runJV b) of
--                       (Ok (I br)) -> do ar <- evalInt a
--                                         br <- evalInt b
--                                         return $ I $ ar + br 
--                       (Ok (F br)) -> do ar <- evalInt a
--                                         br <- evalFloat b
--                                         return $ F $ (fromInteger ar) + br 
--                       _ -> do ar <- evalInt a
--                               br <- evalInt b
--                               err "Parameters of plus must be numbers"
--                     (Ok (F ar)) -> case (runJV b) of 
--                       (Ok (I br)) -> do ar <- evalFloat a
--                                         br <- evalInt b
--                                         return $ F $ ar + (fromInteger br) 
--                       (Ok (F br)) -> do ar <- evalFloat a 
--                                         br <- evalFloat b 
--                                         return $ F $ ar + br
--                       _ -> do ar <- evalInt a
--                               br <- evalInt b
--                               err "Parameters of plus must be numbers"
--                     _ -> do ar <- evalInt a
--                             br <- evalInt b
--                             err "Parameters of plus must be numbers"
eval (Plus a b) =  do ar <- eval a
                      br <- eval b
                      case (ar) of
                        (I ar) -> case br of 
                          (I br) -> return $ I $ ar + br
                          (F br) -> return $ F $ (fromInteger ar) + br
                          _ -> err "Parameters of minus must be numbers"
                        (F ar) -> case br of 
                          (I br) -> return $ F $ ar + (fromInteger br)
                          (F br) -> return $ F $ ar + br
                          _ -> err "Parameters of minus must be numbers"
                        _ -> err "Parameters of minus must be numbers"
-- eval (Minus a b) = do ar <- evalInt a
--                       br <- evalInt b
--                       return $ I $ ar - br

eval (Minus a b) = do ar <- eval a
                      br <- eval b
                      case (ar) of
                        (I ar) -> case br of 
                          (I br) -> return $ I $ ar - br
                          (F br) -> return $ F $ (fromInteger ar) - br
                          _ -> err "Parameters of minus must be numbers"
                        (F ar) -> case br of 
                          (I br) -> return $ F $ ar - (fromInteger br)
                          (F br) -> return $ F $ ar - br
                          _ -> err "Parameters of minus must be numbers"
                        _ -> err "Parameters of minus must be numbers"

eval (Mult a b) =  do ar <- eval a
                      br <- eval b
                      case (ar) of
                        (I ar) -> case br of 
                          (I br) -> return $ I $ ar * br
                          (F br) -> return $ F $ (fromInteger ar) * br
                          _ -> err "Parameters of minus must be numbers"
                        (F ar) -> case br of 
                          (I br) -> return $ F $ ar * (fromInteger br)
                          (F br) -> return $ F $ ar * br
                          _ -> err "Parameters of minus must be numbers"
                        _ -> err "Parameters of minus must be numbers"
eval (DivF a b) =do ar <- eval a
                    br <- eval b
                    case (ar) of
                      (I ar) -> case br of 
                        (I br) -> case (br == 0) of 
                          False -> return $ I $ ar `div` br
                          True -> err "can't divided by zero"
                        (F br) -> case (br == 0) of
                          False -> return $ F $ (fromInteger ar) / br
                          True -> err "can't divided by zero" 
                        _ -> err "Parameters of divided must be numbers"
                      (F ar) -> case br of 
                        (I br) -> case (br == 0) of
                          False -> return $ F $ ar / (fromInteger br)
                          True -> err "can't divided by zero" 
                        (F br) -> case (br == 0) of 
                          False -> return $ F $ ar / br
                          True -> err "can't divided by zero" 
                        _ -> err "Parameters of divided must be numbers"
                      _ -> err "Parameters of divided must be numbers"
eval (DivI a b) =do ar <- eval a
                    br <- eval b
                    case (ar) of
                      (I ar) -> case br of 
                        (I br) -> case (br == 0) of 
                          False -> return $ I $ ar `div` br
                          True -> err "can't divided by zero"
                        _ -> err "Parameters of integer division must be integer"
                      _ -> err "Parameters of integer division must be integer"
-- eval (Div a b) = do ar <- eval a
--                     br <- eval b
--                     case (ar) of
--                       (I ar) -> case br of 
--                         (I br) -> case (br == 0) of 
--                           False -> return $ I $ ar `div` br
--                           True -> err "can't divided by zero"
--                         (F br) -> case (br == 0) of
--                           False -> return $ F $ (fromInteger ar) / br
--                           True -> err "can't divided by zero" 
--                         _ -> err "Parameters of minus must be numbers"
--                       (F ar) -> case br of 
--                         (I br) -> case (br == 0) of
--                           False -> return $ F $ ar / (fromInteger br)
--                           True -> err "can't divided by zero" 
--                         (F br) -> case (br == 0) of 
--                           False -> return $ F $ ar / br
--                           True -> err "can't divided by zero" 
--                         _ -> err "Parameters of minus must be numbers"
--                       _ -> err "Parameters of minus must be numbers"
eval (Mod a b) = do ar <- eval a 
                    br <- eval b 
                    case ar of 
                      (I a') -> case br of 
                        (I b') -> return $ I $ a' `mod` b' 
                        _ -> err "Parameters of mod must be Integers"
                      _ -> err "Parameters of mod must be Integers"
eval (ExpF a b) =do ar <- eval a 
                    br <- eval b 
                    case ar of 
                      (F a') -> case br of 
                        (F b') -> return $ F $ a' ** b'
                        _ -> err "Exp for floats must have all floats"
                      _ -> err "Exp for floats must have all floats"
eval (ExpI a b) =do ar <- eval a 
                    br <- eval b 
                    case ar of 
                      (I a') -> case br of 
                        (I b') -> return $ I $ a' ^ b'
                        _ -> err "Exp for ints must have all ints"
                      _ -> err "Exp for ints must have all ints"
-- eval (Exp a b) = do ar <- eval a 
--                     br <- eval b 
--                     case ar of 
--                       (I a') -> case br of 
--                         (I b') -> return $ I $ a' ^ b'
--                         _ -> err "Exp must have all integers or floats"
--                       (F a') -> case br of 
--                         (F b') -> return $ F $ a' ** b'
--                         _ -> err "Exp must have all integers or floats"
--                       _ -> err "Exp must have all integers or floats"
eval (Ind (Nil) _) = do err "Can't find index in an empty list"
eval (Ind (Cons h t) i) =  do ir <- eval i 
                              l <- eval (Cons h t)
                              case (ir, l) of 
                                (I i', Ls l') -> return $ l' !! (fromInteger i')
                                _ -> err "List index must be an integer"
                              
eval (Ind _ _) = do err "Can only list index a list"
eval (UMinus a) = do ar <- eval a 
                     case ar of 
                      (I a') -> return $ I $ 0 - a'
                      (F a') -> return $ F $ 0 - a' 
                      _ -> err "Unary minus only takes numbers"
eval (Nil) = return $ Ls []
eval (Cons a b) =  do list <- evalList (Cons a b)
                      return $ Ls $ list
eval (If a b c) =  do ar <- evalBool a
                      br <- eval b
                      cr <- eval c
                      case (ar) of
                        True -> eval b
                        False -> eval c
eval (Let s a b) = do ar <- eval a
                      env <- getEnv
                      case runEnvUnsafeLog (eval b) (Map.insert s ar env) of
                        (Ok a, log1) -> EnvUnsafeLog $ \env -> (Ok a, log1)
                        (Error e, log2) -> err e
eval (Var v t) = do env <- getEnv
                    case (Map.lookup v env) of
                      Just a -> return a
                      Nothing -> err $ "Var "++v++" is not in scope"
eval (Lam s t1 a) =do env <- getEnv
                      return $ Fun $ \ v -> let (unsafe, log) = runEnvUnsafeLog (eval a) $ Map.insert s v env in 
                        unsafe
eval (App a b) = do f <- evalFun a
                    br <- eval b
                    case (f br) of
                      Ok r -> return r
                      Error e -> err e
eval (Print a) = do a' <- eval a
                    EnvUnsafeLog $ \env -> (Ok a', [show a'])
                    
eval (Sep a b) = do a' <- eval a 
                    b' <- eval b 
                    EnvUnsafeLog $ \env -> (Ok b', [])
eval (Ast.EQ a b) =  do a' <- eval a 
                        b' <- eval b 
                        case (a') of 
                          (Fun f) -> err "No instance of eq for Functions"
                          _ -> case (b') of 
                            (Fun f) -> err "No instance of eq for Functions"
                            _ -> valEq a' b'
eval (NEQ a b) = eval $ Not (Ast.EQ a b)
eval (Ast.GT a b) =  do a' <- eval a 
                        b' <- eval b 
                        valGT a' b'
eval (Ast.LT a b) = do r <- eval $ Not (Ast.EQ a b)
                       r2 <- eval $ Not (Ast.GT a b)
                       case r of
                        (B a') -> case r2 of 
                          (B b') -> return $ B $ a' && b'
                          _ -> err "Error with less than"
                        _ -> err "Error with less than"
eval (LTE a b) = do r <- eval $ Ast.LT a b 
                    r2 <- eval $ Ast.EQ a b 
                    case r of
                     (B a') -> case r2 of 
                       (B b') -> return $ B $ a' || b'
                       _ -> err "Error with less than or eq"
                     _ -> err "Error with less than or eq"
eval (GTE a b) = do r <- eval $ Ast.GT a b 
                    r2 <- eval $ Ast.EQ a b 
                    case r of
                     (B a') -> case r2 of 
                       (B b') -> return $ B $ a' || b'
                       _ -> err "Error with greater than or eq"
                     _ -> err "Error with greater than or eq"                   
eval (Concat a b) = do 
  ar <- evalList a 
  br <- evalList b 
  return $ Ls $ ar ++ br

                    
valEq :: Val -> Val -> EnvUnsafeLog Env Val 
valEq (Fun _) _ = err "No instance of eq for Functions"
valEq _ (Fun _) = err "No instance of eq for Functions"
valEq (I i1) (I i2) = return $ B $ i1 == i2 
valEq (B b1) (B b2) = return $ B $ b1 == b2 
valEq (F f1) (F f2) = return $ B $ f1 == f2 
valEq (S s1) (S s2) = return $ B $ s1 == s2 
valEq (C c1) (C c2) = return $ B $ c1 == c2 
valEq (Ls l1) (Ls l2) = lsEq l1 l2
valEq _ _ = return $ B False

lsEq :: [Val] -> [Val] -> EnvUnsafeLog Env Val 
lsEq [] [] = return $ B $ True 
lsEq _ [] = return $ B $ False 
lsEq [] _ = return $ B $ False
lsEq (x:xs) (y:ys) = do r <- (valEq x y) 
                        case r of 
                          (B True) -> (lsEq xs ys)
                          _ -> return $ B $ False 

-- eq is different. it compares everything in lists until fail.
-- greater than finds the first element that is greater, and then return
valGT :: Val -> Val -> EnvUnsafeLog Env Val 
valGT (Fun _) _ = err "No instance of compare for Functions"
valGT _ (Fun _) = err "No instance of compare for Functions"
valGT (I i1) (I i2) = return $ B $ i1 > i2 
valGT (B b1) (B b2) = return $ B $ b1 > b2 
valGT (F f1) (F f2) = return $ B $ f1 > f2 
valGT (S s1) (S s2) = return $ B $ s1 > s2 
valGT (C c1) (C c2) = return $ B $ c1 > c2 
valGT (Ls l1) (Ls l2) = lsGT l1 l2
valGT (I i) (F f) = return $ B $ (fromInteger i) > f 
valGT (F f) (I i) = return $ B $ f > (fromInteger i)
valGT _ _ = err "Can't compare those objects"

lsGT :: [Val] -> [Val] -> EnvUnsafeLog Env Val 
lsGT [] [] = return $ B $ False 
lsGT _ [] = return $ B $ True
lsGT [] _ = return $ B $ False
lsGT (x:xs) (y:ys) = do r <- (valGT x y) 
                        r2 <- (valEq x y)
                        case r of 
                          (B True) -> return $ B $ True 
                          _ -> case r2 of 
                            (B True) -> (lsGT xs ys)
                            (B False) -> return $ B $ False 
                            (_) -> err "ValEq didn't give bool"
 
val0 = I 3
val1 = I 5
val2 = F 3.14
val3 = F 9.99
val4 = S "string"
val5 = C 'h'
val6 = B True 
val7 = Ls [val0, val3, val4]
val8 = Fun $ \v -> case v of -- filter that keeps integers
  (I _) -> Ok (B True)
  _ -> Ok (B False)
val9 = Ls []

std0 = runStdLib "head" val7 -- Ok val0
std1 = runStdLib "head" val9 -- Error "can only call head on a non empty list"
std2 = runStdLib "tail" val7 -- Ok (Ls [val3, val4])
std3 = run2StdLib "elem" val3 val7 -- Ok (B True)
std4 = run2StdLib "filter" val8 val7 -- Ok (Ls [val0])
std5 = (runStdLib "ord" val5) 
std6 = (runStdLib "chr" val0) 
std7 = (runStdLib "float" val1) -- Ok (F 5.0)
std8 = (runStdLib "int" val3) -- Ok (I 10)
std9 = runStdLib "float" val8 -- Error "Float takes integer"
std10 = runStdLib "int" val8 -- Error "Int takes float"