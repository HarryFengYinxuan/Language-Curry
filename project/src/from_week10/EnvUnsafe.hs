module EnvUnsafe where

import Control.Monad(ap)

--This monad will form the plumbing for the evaluation function

data Unsafe a = Error String | Ok a deriving (Show, Eq)

data EnvUnsafe e a = EnvUnsafe (e -> Unsafe a )

-- function that just runs the function contained in EnvUnsafe
runEnvUnsafe ::  (EnvUnsafe e a) -> e -> Unsafe a
runEnvUnsafe (EnvUnsafe eu) e = eu e

-- a way to easily return an error (for instance in do notation)
err :: String -> EnvUnsafe e a
err s = EnvUnsafe $ \ _ -> Error s


-- a way to easily get the entire environment (for instance in do notation)
getEnv :: EnvUnsafe e e
getEnv = EnvUnsafe $ \ env -> Ok env


instance Functor (EnvUnsafe e) where
  -- fmap :: (a -> b) -> EnvUnsafe a -> EnvUnsafe b
  fmap f (EnvUnsafe eu) = EnvUnsafe $ \e -> case (runEnvUnsafe (EnvUnsafe eu) e) of
                                            Ok a -> Ok (f a)
                                            Error s -> Error s
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (EnvUnsafe e) where
  pure = return
  (<*>) = ap

instance Monad (EnvUnsafe e) where
  --return :: a -> EnvUnsafe a
  return a = EnvUnsafe $ \e -> Ok a

  --(>>=) :: EnvUnsafe a -> (a -> EnvUnsafe b) -> EnvUnsafe b
  (EnvUnsafe eu) >>= f = EnvUnsafe $ \e -> case (runEnvUnsafe (EnvUnsafe eu) e) of
                                           Ok a -> runEnvUnsafe (f a) e
                                           Error s -> Error s

  -- make sure your implementation follows the Monad laws

-- technical note: this could be done more succinctly with monad transformers, but it is also good practice to do it by hand
