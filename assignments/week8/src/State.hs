module State where
import Control.Monad(ap)

-- see lecture notes for this file

--This monad will form the plumbing for the evaluation function of lang3

data State s a  = State (s -> (a, s))

-- a helper function to pull out the function bit
runState :: State s a -> (s -> (a, s))
runState (State st) = st


instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State st) = State $ \s -> let (a, s') = (st s) in
                        let b = (f a) in
                        (b, s')

--ignore this for now
instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  --return :: a -> State s a
  return a = State (\x -> (a, x))

  --(>>=) :: State s a -> (a -> State s b) -> State s b
  (State st) >>= f = State $ \s -> let (a, s') = (st s) in
                           let (State st') = (f a) in
                           st' s'



-- a function that gets the state (in a stateful way)
-- stolen from https://wiki.haskell.org/State_Monad
put :: s -> State s ()
put s = State $ \ _ ->  ((), s)


get :: State s s
get = State $ \ s ->  (s, s)
