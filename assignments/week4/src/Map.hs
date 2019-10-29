module Map
  (Map(),empty, insert, toList, fromList, size, member, lookup, delete, update, union, filter,
  null, singleton, elems, keys, difference, adjust, alter
  ) where  -- DO NOT EDIT THESE LINES
import Prelude hiding (filter, lookup, null)


-- For this problem, you must develop a variation of a dictionary based on BST.
-- Such a data structure is very useful in a wide variety of applications.


data Map k v = Null | Node (Map k v) k v (Map k v)  deriving Show

-- The empty map.
empty :: Map k a
empty = Null

-- Insert a new key and value in the map. If the key is already present in the map, the associated value is replaced with the supplied value
insert :: Ord k => k -> a -> Map k a -> Map k a
insert k v (Null) = Node Null k v Null
insert k v (Node h mk mv t) = case (k == mk) of
                                True -> (Node h mk v t)
                                False -> case (k < mk) of
                                  True -> (Node (insert k v h) mk mv t)
                                  False -> (Node h mk mv (insert k v t))

--  Convert to a list of key/value pairs.
toList :: Map k a -> [(k, a)]
toList Null = []
toList (Node a k v b) = (toList a) ++ [(k, v)] ++ (toList b)

-- Build a map from a list of key/value pairs.
fromList :: Ord k => [(k, a)] -> Map k a
fromList [] = Null
fromList ((k, v):xs) = insert k v (fromList xs)

-- The number of elements in the map.
size :: Map k a -> Int
size Null = 0
size (Node a k v b) = (size a) + 1 + (size b)

-- Is the key a member of the map?
member :: Ord k => k -> Map k a -> Bool
member k Null= False
member k (Node a mk mv b) = case (k == mk) of
                              True -> True
                              False -> (member k a) || (member k b)

-- Lookup the value at a key in the map.
-- The function will return the corresponding value as (Just value), or Nothing if the key isn't in the map.
lookup :: Ord k => k -> Map k a -> Maybe a
lookup k Null = Nothing
lookup k (Node a mk mv b) = case (k == mk) of
                              True -> Just mv
                              False -> case (lookup k a) of
                                Just value -> Just value
                                Nothing -> case (lookup k b) of
                                  Just value -> Just value
                                  Nothing -> Nothing

-- Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
dhelper :: Ord k => k -> [(k, a)] -> [(k, a)]
dhelper k [] = []
dhelper k ((mk, mv):xs) = case (k==mk) of
                            True -> xs
                            False -> [(mk, mv)]++(dhelper k xs)

delete :: Ord k => k -> Map k a -> Map k a
delete k Null = Null
delete k (Node a mk mv b) = fromList (dhelper k (toList (Node a mk mv b)))

-- The expression (update f k map) updates the value x at k (if it is in the map). If (f x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y.
update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k Null = Null
update f k (Node a mk mv b) = case (mk==k) of
                                True -> case (f mv) of
                                  Nothing -> delete k (Node a mk mv b)
                                  Just value -> (Node a mk value b)
                                False -> (Node (update f k a) mk mv (update f k b))

--  The expression (union t1 t2) takes the left-biased union of t1 and t2. It prefers t1 when duplicate keys are encountered, i.e. (union == unionWith const).
union :: Ord k => Map k a -> Map k a -> Map k a
union Null a = a
union a Null = a
union (Node a1 mk1 mv1 b1) (Node a2 mk2 mv2 b2) = union a1 (union b1 (insert mk1 mv1 (Node a2 mk2 mv2 b2)))

-- Filter all values that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter f Null = Null
filter f (Node a mk mv b) = case (f mv) of
                              False -> filter f (delete mk (Node a mk mv b))
                              True -> Node (filter f a) mk mv (filter f b)



instance (Ord k, Eq v) => Eq (Map k v) where
  Null == Null = True
  Null == _ = False
  _ == Null = False
  a == b = (toList a) == (toList b)


instance Functor (Map k) where
-- Map a function over all values in the map.
--  fmap ::  (a -> b) -> Map k a -> Map k b
  fmap f Null = Null
  fmap f (Node a mk mv b) = (Node (fmap f a) mk (f mv) (fmap f b))



-- ungraded bonus


-- Is the map empty?
null :: Map k a -> Bool
null Null = True
null _ = False

-- A map with a single element.
singleton :: k -> a -> Map k a
singleton k v = Node Null k v Null

-- Return all elements of the map in the ascending order of their keys.
elems :: Map k a -> [a]
elems Null = []
elems (Node a mk mv b) = (elems a) ++ [mv] ++ (elems b)

-- Return all keys of the map in ascending order.
keys :: Map k a -> [k]
keys Null = []
keys (Node a mk mv b) = (keys a) ++ [mk] ++ (keys b)

-- Difference of two maps. Return elements of the first map not existing in the second map.
difference :: Ord k => Map k a -> Map k b -> Map k a
difference Null Null = Null
difference Null a = Null
difference a Null = a
difference (Node a1 mk1 mv1 b1) (Node a2 mk2 mv2 b2) = case (mk1==mk2) of
                  True -> let d = (difference a1 a2) in case (null d) of
                    True -> let d2 = (difference b1 b2) in case (null d2) of
                      True -> empty
                      False -> d2
                    False -> d
                  False -> (Node a1 mk1 mv1 b1)

-- Update a value at a specific key with the result of the provided function. When the key is not a member of the map, the original map is returned.
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f k Null = Null
adjust f k (Node a mk mv b) = update (\x -> Just (f x)) k (Node a mk mv b)

-- The expression (alter f k map) alters the value x at k, or absence thereof. alter can be used to insert, delete, or update a value in a Map. In short : lookup k (alter f k m) = f (lookup k m).
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k Null = Null
alter f k (Node a mk mv b) = case (member k (Node a mk mv b)) of
                                True -> let func = \x -> f (Just x) in update func k (Node a mk mv b)
                                False -> case (f Nothing) of
                                  Just value -> insert k value (Node a mk mv b)
                                  _ -> (Node a mk mv b)


-- instance (Ord k, Ord v) => Ord (Map k v) where
