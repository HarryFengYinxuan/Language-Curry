module LambdaCalcImplementation where

import Data.Set (Set)
import qualified Data.Set as Set

data Term = FreeVar String
          | App Term Term
          | Lam String Term

-- 3 helper functions for the graders

-- you will probably need the following helper functions
boundVars :: Term -> Set String
boundVars (Lam v bod) = Set.insert v $ boundVars bod
boundVars (FreeVar _) = Set.empty
boundVars (App f a) = boundVars f `Set.union` boundVars a

-- makes a free var from a string
mkFreeVar :: String -> Term
mkFreeVar s = FreeVar s

-- applies the 2nd term to the first
mkApp  ::  Term -> Term -> Term
mkApp t1 t2 = App t1 t2

-- creates a lambda out of a term and the name of a free var

bindToLam :: String -> Term -> Term
bindToLam v t = Lam (v) t


-- true if there no evaluation that can be done in the lambda expression
isValue :: Term -> Bool
isValue (App (Lam _ _) _)= False
isValue (FreeVar _) = True
isValue (App a b) = (isValue a) && (isValue b)
isValue (Lam s t) = isValue t

-- collect all the vars that appear in the expression that are not bound
freeVars :: Term -> Set String
freeVars (FreeVar s) = Set.singleton s
freeVars (App t1 t2) = (freeVars t1) `Set.union` (freeVars t2)
freeVars (Lam s t2) = (freeVars t2) `Set.difference` (Set.singleton s)

-- when are there no free variables in a lambda expression?
isClosed :: Term -> Bool
isClosed t = null (freeVars t)


findName :: String -> Set String -> String
findName str avoidNames | Set.member str avoidNames = findName (str ++ "'") avoidNames
                        | otherwise                 = str


-- rename a free var, in a term
rename :: Term -> String -> String -> Term
rename (App f a)      from to             = App (rename f from to) (rename a from to)
rename (Lam v bod)    from to | v == from = Lam v bod
rename (Lam v bod)    from to | otherwise = Lam v $ rename bod from to
rename (FreeVar v)        from to | v == from = FreeVar to
                              | otherwise = FreeVar v

-- Apply a substitution to a term, being careful in any alpha-conversions to avoid a given set of variables
subst :: Term -> String -> Term -> Set String -> Term
subst (App f a)      from to avoid             = App (subst f from to avoid) (subst a from to avoid)
subst (FreeVar v)        from to _ | v == from = to
                               | otherwise = FreeVar v
subst (Lam v bod)    from to avoid | v == from = Lam v bod
                                   | otherwise =
  let v' = findName v avoid
      bod' = rename bod v v'
  in Lam v' $ subst bod' from to (Set.insert v' avoid)



-- do all possible applications, rename bound variables as needed
eval :: Term -> Term
eval (App (Lam v t1) t2) = case (Set.null ((boundVars (Lam v t1)) `Set.difference` (freeVars t2))) of
                           True -> let r = subst t1 v (eval t2) Set.empty in
                                   case (isValue r) of
                                    True -> r
                                    False -> eval r
                           False ->  let t2' = rename t2 v $ findName v ((boundVars (Lam v t1)) `Set.difference` (freeVars t2)) in
                                     let r = subst t1 v (eval t2') ((boundVars (Lam v t1)) `Set.difference` (freeVars t2)) in
                                     case (isValue r) of
                                      True -> r
                                      False -> eval r
eval (App t1 t2) =  App (eval t1) (eval t2)
eval (FreeVar s) = FreeVar s
eval (Lam v t) = Lam v (eval t)



-- show the unevaluated expression, in a human readable way.  It must be compatible with your parser.
-- you may choose a to show the fully parenthesized version
instance Show Term where
  show (FreeVar s) = s
  show (Lam v t) = "(\\"++v++" -> " ++ (show t) ++ ")"
  show (App t1 t2) = (show t1)++" "++(show t2)

-- equality on the structure of the term, where the names of bindings don't matter.
-- this is called alpha-equality
instance Eq Term where
  (FreeVar s1) == (FreeVar s2) = s1 == s2
  (App t1 t2) == (App t3 t4) = t1 == t3 && t2 == t4
  (Lam v1 t1) == (Lam v2 t2) = let avoid = ((freeVars t1) `Set.union` (freeVars t2)) in
                               let avoid2 = Set.insert v1 avoid in
                               let avoid3 = Set.insert v2 avoid2 in
                               let newName = findName v1 avoid3 in
                               let newT1 = rename t1 v1 newName in
                               let newT2 = rename t2 v2 newName in
                               newT1 == newT2
  _ == _ = False
