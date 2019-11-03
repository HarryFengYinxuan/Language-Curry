module Check where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad(ap)
import Ast
import qualified Data.Map as Map
import Data.Map (Map)

-- here you can preform static checks


data Ok a = Ok a | ErrOk String deriving (Eq, Show)

instance Functor Ok where
    fmap f (ErrOk x) = ErrOk x
    fmap f (Ok a) = Ok (f a)

instance Applicative Ok where
    pure = return 
    (<*>) = ap

instance Monad Ok where
    return x = Ok x
    mx >>= f = case mx of
                     ErrOk a -> ErrOk a
                     Ok x -> f x
-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors
data WarningMsg = 
    UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name
    | FineGram
    | WrongType String
  -- ...
  deriving (Show,Eq,Ord)

getType :: Ast -> Ok Type
getType Nil = return ListType
getType (Var _ t) = return t 
getType (Let v n b) = getType b
getType (ValInt _ ) = return IntegerType
getType (ValFloat _ ) = return FloatType
getType (ValBool _ ) = return BoolType
getType (ValChar _ ) = return CharType
getType (ValStr _ ) = return StringType
getType (Print p) = case getType p of 
  (Ok _) -> return Void 
  (ErrOk e) -> ErrOk e
getType (UMinus x) = case (getType x) of 
                      (Ok IntegerType) -> return IntegerType
                      (Ok FloatType) -> return FloatType
                      _ -> ErrOk $ "Type Error for " ++ show (UMinus x)
getType (Plus x y) =
  case (getType x, getType y) of
     (Ok IntegerType, Ok IntegerType) -> return IntegerType
     (Ok FloatType, Ok FloatType) -> return FloatType
     (Ok FloatType, Ok IntegerType) -> return FloatType
     (_ , _ ) -> ErrOk $ "Type Error for " ++ show (Plus x y)
getType (Minus x y) =
  case (getType x, getType y) of
     (Ok IntegerType, Ok IntegerType) -> return IntegerType
     (Ok FloatType, Ok FloatType) -> return FloatType
     (Ok FloatType, Ok IntegerType) -> return FloatType
     (_ , _ ) -> ErrOk $ "Type Error for " ++ show (Minus x y)
getType (Mult x y) =
  case (getType x, getType y) of
     (Ok IntegerType, Ok IntegerType) -> return IntegerType
     (Ok FloatType, Ok FloatType) -> return FloatType
     (Ok FloatType, Ok IntegerType) -> return FloatType
     (Ok IntegerType, Ok FloatType) -> return FloatType
     (_ , _ ) -> ErrOk $ "Type Error for " ++ show (Mult x y)
getType (DivF x y) =
  case (getType x, getType y) of
     (Ok IntegerType, Ok IntegerType) -> return IntegerType
     (Ok FloatType, Ok FloatType) -> return FloatType
     (Ok FloatType, Ok IntegerType) -> return FloatType
     (_ , _ ) -> ErrOk $ "Type Error for " ++ show (DivF x y)
getType (DivI x y) =
  case (getType x, getType y) of
    (Ok IntegerType, Ok IntegerType) -> return IntegerType
    (_ , _ ) -> ErrOk $ "Type Error for " ++ show (DivI x y)
getType (Mod x y) = case (getType x, getType y) of 
                      (Ok IntegerType, Ok IntegerType) -> return IntegerType
                      _ -> ErrOk $ "Type Error for " ++ show (Mod x y)
getType (ExpI x y) = case (getType x, getType y) of 
                      (Ok IntegerType, Ok IntegerType) -> return IntegerType
                      _ -> ErrOk $ "Type Error for " ++ show (ExpI x y)
getType (ExpF x y) = case (getType x, getType y) of 
                      (Ok FloatType, Ok FloatType) -> return FloatType
                      _ -> ErrOk $ "Type Error for " ++ show (ExpF x y)

getType (And x y) = 
  case (getType x, getType y) of
     (Ok BoolType, Ok BoolType) -> return BoolType
     (_ , _ ) -> ErrOk $ "Type Error for " ++ show (And x y)
getType (Or x y) = 
  case (getType x, getType y) of
     (Ok BoolType, Ok BoolType) -> return BoolType
     (_ , _ ) -> ErrOk $ "Type Error for " ++ show (Or x y)
getType (Not x) = getType x
getType (Ind x y) = case (getType x, getType y) of 
                      (Ok ListType, Ok IntegerType) -> case x of
                                                        (Cons h t) -> case y of 
                                                          (ValInt i) -> getType (listIndex (Cons h t) i)
                                                          _ -> ErrOk "getType went wrong"
                                                        _ -> ErrOk "getType went wrong"
                      (Ok ListType, Ok _) -> ErrOk "Second argument of list index must be integer type" 
                      (Ok _, Ok _) -> ErrOk "First argument of list index must be list type" 
                      (Ok _, ErrOk s) -> ErrOk s 
                      (ErrOk s, _) -> ErrOk s
getType (Cons x y) = 
  case (listSameType (Cons x y)) of
    (True, _) -> return ListType
    (False, s) -> ErrOk s
getType (Concat x y) =
  case (getType x, getType y) of 
    (Ok ListType, Ok ListType) -> Ok ListType
    (Ok _, Ok _) -> ErrOk "Must concat two lists"
    (Ok _, ErrOk s) -> ErrOk s
    (ErrOk s, _) -> ErrOk s
getType (If x y z) = 
  case (getType x, getType y, getType z) of 
    (Ok BoolType, Ok a, Ok b) -> case (a == b) of 
                                  True -> Ok a 
                                  False -> ErrOk "If must have same types for then and else"
    (Ok _, Ok _, Ok _) -> ErrOk "First argument of if must be bool type"
    (ErrOk s, _, _) -> ErrOk s 
    (Ok _, ErrOk s, _) -> ErrOk s
    (Ok _, Ok _, ErrOk s) -> ErrOk s
getType (Lam v t b) = case (getType b) of 
  (Ok t2) -> case t == t2 of 
    (True) -> return FuncType
    _ -> ErrOk "Lam return type doesn't match"
  (ErrOk s) -> ErrOk s
getType (App a b) = case a of 
  (Lam v t b2) -> case (getType b == (Ok t)) of 
    (True) -> case Map.lookup v (freeVars b2) of 
      (Just t2) -> case (Ok t2) == getType b of 
        True -> return t 
        False -> ErrOk "Input of function type mismatch"
      Nothing -> return t
    False -> ErrOk "Function return type mismatch"
  _ -> ErrOk "First argument of App is not a function"
getType (Sep a b) = getType b
getType (Ast.NEQ a b) = case (getType a == getType b) of 
  True -> return BoolType
  _ -> ErrOk "Arguments of compaision needs the same type"
getType (Ast.EQ a b) = case (getType a == getType b) of 
  True -> return BoolType
  _ -> ErrOk "Arguments of compaision needs the same type"
getType (Ast.LT a b) = case (getType a == getType b) of 
  True -> return BoolType
  _ -> ErrOk "Arguments of compaision needs the same type"
getType (Ast.LTE a b) = case (getType a == getType b) of 
  True -> return BoolType
  _ -> ErrOk "Arguments of compaision needs the same type"
getType (Ast.GT a b) = case (getType a == getType b) of 
  True -> return BoolType
  _ -> ErrOk "Arguments of compaision needs the same type"
getType (Ast.GTE a b) = case (getType a == getType b) of 
  True -> return BoolType
  _ -> ErrOk "Arguments of compaision needs the same type"

listOk :: Ast -> (Bool, String)
listOk (Nil) = (True, "")
listOk (Cons x y) = case (getType x) of 
                       (Ok _) -> listOk y 
                       (ErrOk s) -> (False, s)
listOk _ = (False, "Not a list")

listSameType :: Ast -> (Bool, String)
listSameType (Nil) = (True, "")
listSameType (Cons x y) = case (listOk (Cons x y)) of 
                            (True, _) -> case y of 
                              (Cons z _) -> let (yb, _) = listSameType y in
                                            (((getType x) == getType z) && (yb), "")
                              (Nil) -> (True, "")
                              _ -> (False, "Error with list same type")
                                         
                            (False, s) -> (False, s)
listSameType _ = (False, "Not a list")

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check ast = case (getType ast) of 
  (Ok _) -> (checkUnbound ast)
  (ErrOk s) -> Set.delete FineGram (Set.insert (WrongType s) (checkUnbound ast)) 

checkUnbound :: Ast -> Set WarningMsg
checkUnbound ast = case (freeVars ast == Map.empty) of 
  True -> Set.singleton FineGram
  _ -> Set.fromList (map (\x-> UndefinedVarUse (x ++ " is not in scope")) (Map.keys (freeVars ast)))

freeVars :: Ast -> Map String Type 
freeVars (Var s t) = Map.fromList [(s, t)] 
freeVars (Lam v t b) = Map.delete v (freeVars b) 
freeVars (Let v n b) = Map.delete v (freeVars b) 

freeVars (ValBool _) = Map.empty
freeVars (ValInt _) = Map.empty
freeVars (ValFloat _) = Map.empty
freeVars (ValChar _) = Map.empty
freeVars (ValStr _) = Map.empty
freeVars (Nil) = Map.empty


freeVars (And a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Or a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Plus a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Minus a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Mult a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (DivI a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (DivF a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Mod a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (ExpI a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (ExpF a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Ind a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Cons a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Concat a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (App a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Sep a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Ast.EQ a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (NEQ a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Ast.LT a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (LTE a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (Ast.GT a b) = (freeVars a) `Map.union` (freeVars b)
freeVars (GTE a b) = (freeVars a) `Map.union` (freeVars b)

freeVars (If t a b) = (freeVars a) `Map.union` (freeVars b)

freeVars (Not a) = (freeVars a)
freeVars (Print a) = (freeVars a)
freeVars (UMinus a) = (freeVars a)
