module Set(Set(),
  size,
  empty,
  singleton,
  insert,
  fromList,
  delete,
  member,
  elems
   ) where

import Prelude (Show,undefined,Bool(True,False),(&&),(||),not,
                Integer,(==),(/=),(<),(>),(<=),(>=),(+),(-),(*),div,mod)
import qualified  Prelude ((++), show)
import Hw02 hiding (insert)


-- a set based on search trees
-- note that the implementation details are hidden inside the module
-- this means that outside the module a bad Set Cannot be defined

data Set e = Set (Ordering e) (SetInner e)

data SetInner e = Null | Node (SetInner e) e (SetInner e) deriving Show

-- someone outside the module can make good sets from the functions we provide
exampleGood :: Set Integer
exampleGood = insert 3 ( insert 2 ( insert 1 (empty intOrd)))

-- only possible to make these mistakes inside the module
exampleBad :: Set Integer
exampleBad = Set intOrd (Node (Node Null 3 Null) 2 (Node Null 2 Null) )


-- The number of elements in the set.

sizeHelper :: SetInner e -> Integer
sizeHelper Null = 0
sizeHelper (Node a b c) = 1 + (sizeHelper a) + (sizeHelper c)

size :: Set e -> Integer
size (Set _ a)= sizeHelper a

-- The empty set.
empty ::  (Ordering e) -> Set e
empty  a = Set a Null

-- Create a singleton set.
singleton :: Ordering e -> e -> Set e
singleton a b = Set a (Node Null b Null)


-- Insert an element in a set. If the set already contains an element equal to the given value, it is replaced with the new value.
insertInner :: e -> Ordering e -> SetInner e -> SetInner e
insertInner a (Ordering o) Null = (Node Null a Null)
insertInner a (Ordering o) (Node b c d)
    | (comComp (o a c) GreaterThan) = (Node b c (insertInner a (Ordering o) d))
    | comComp (o a c) LessThan = (Node (insertInner a (Ordering o) b) c d)
    | True = (Node b c d)

insert :: e -> Set e -> Set e
insert a (Set (Ordering o) b) = (Set (Ordering o) (insertInner a (Ordering o) b))

-- Create a set from a list of elements.
fromList :: Ordering e -> List e -> Set e
fromList o Nil = Set (o) Null
fromList o (Cons h t) = insert h (fromList o t)

--  The elements of a set in ascending order.

concatenate :: (List a) -> (List a) -> (List a)
concatenate Nil Nil = Nil
concatenate (Cons a b) Nil = (Cons a b)
concatenate Nil (Cons a b) = (Cons a b)
concatenate (Cons a Nil) (Cons b c) = concatenate (Cons a (Cons b Nil)) c
concatenate (Cons a (Cons b c)) (Cons d e) = Cons a (Cons b (concatenate c (Cons d e)))

ehelper :: SetInner e -> List e
ehelper Null = Nil
ehelper (Node Null a Null) = Cons a Nil
ehelper (Node a b c) = concatenate (ehelper a) (Cons b (ehelper c))

elems  :: Set e -> List e
elems (Set (Ordering f) Null) = Nil
elems (Set (Ordering f) (Node a b c)) = ehelper (Node a b c)

memhelper :: (Ordering e) -> e -> SetInner e -> Bool
memhelper (Ordering f) a Null = False
memhelper (Ordering f) a (Node Null b Null) = comComp (f a b) EqualTo
memhelper (Ordering f) a (Node b c d) = comComp (f a c) EqualTo || (memhelper (Ordering f) a b) || (memhelper (Ordering f) a d)

-- Is the element in the set?
member :: e -> Set e -> Bool
member a (Set f iset)= memhelper f a iset


delList :: (Ordering a) -> a -> List a -> List a
delList (Ordering f) a Nil = Nil
delList (Ordering f) a (Cons h t) = case (f a h) of
                                    EqualTo -> t
                                    _ -> Cons h (delList (Ordering f) a t)
-- ungraded challenge problem:
-- Delete an element from a set.
delete :: e -> Set e -> Set e
delete a (Set o iset) = fromList o (delList o a (elems (Set o iset)))


-- ignore this for now, just so it is pretty in your repl
instance Show e => Show (Set e) where
  show (Set _ inner) = (Prelude.++) ((Prelude.++) "Set ??? ("  (Prelude.show  inner)) ")"
