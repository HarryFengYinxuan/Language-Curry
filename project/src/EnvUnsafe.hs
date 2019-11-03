module EnvUnsafe where

import Control.Monad(ap)

data Unsafe a = Error String | Ok a deriving (Show, Eq)

data EnvUnsafeLog env a = EnvUnsafeLog (env -> (Unsafe a, [String])) 

instance Functor (EnvUnsafeLog e) where
  fmap f (EnvUnsafeLog e) = EnvUnsafeLog $ \env -> case (e env) of
                                                    (Ok a, log) -> (Ok (f a), log)
                                                    (Error s, log) -> (Error s, log)

instance Applicative (EnvUnsafeLog e) where
  pure = return
  (<*>) = ap
  
instance Monad (EnvUnsafeLog e) where
  return a = EnvUnsafeLog $ \env -> (Ok a, [])
  
  -- (>>=) :: (EnvUnsafeLog e) -> ((Unsafe a, [String]) -> EnvUnsafeLog e) -> EnvUnsafeLog e
  (EnvUnsafeLog e) >>= f = EnvUnsafeLog $ \env -> let (unsafe, log1) = (e env) in
                                                    case (unsafe) of
                                                        (Error s) -> (Error s, log1)
                                                        (Ok a) -> let (EnvUnsafeLog e2) = (f a) in
                                                            let (unsafe, log2) = e2 env in
                                                                case (unsafe) of
                                                                    (Error s) -> (Error s, log1 ++ log2)
                                                                    (Ok a) -> (Ok a, log1 ++ log2)


err :: String -> EnvUnsafeLog e a
err s = EnvUnsafeLog $ \ _ -> (Error s, [])

runEnvUnsafeLog ::  (EnvUnsafeLog e a) -> e -> (Unsafe a, [String])
runEnvUnsafeLog (EnvUnsafeLog eu) e = eu e

{-

runEnvUnsafeLog ::  (EnvUnsafeLog e a) -> e -> [String] -> (Unsafe a, [String])
runEnvUnsafeLog (EnvUnsafeLog eu) e log = let (a, log') = eu e in (a, log ++ log')

-}
getEnv :: EnvUnsafeLog e e
getEnv = EnvUnsafeLog $ \ env -> (Ok env, [])

getLog :: EnvUnsafeLog e a -> e -> [String]
getLog (EnvUnsafeLog e) env = let (_, log) = e env in log