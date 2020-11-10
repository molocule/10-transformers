{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Transformers where

import Control.Monad (ap, liftM)
import State (State) -- State monad developed in lecture
import qualified State as S

data Expr
  = Val Int
  | Div Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

ok :: Expr
ok =
  (Val 1972 `Div` Val 2)
    `Div` Val 23

err :: Expr
err =
  Val 2
    `Div` ( Val 1
              `Div` (Val 2 `Div` Val 3)
          )

evalDefault :: Expr -> Int
evalDefault (Val n) = n
evalDefault (Div x y) =
  let m = evalDefault y
   in if m == 0 then 0 else evalDefault x `div` m

evalMaybe :: Expr -> Maybe Int
evalMaybe (Val n) = return n
evalMaybe (Div x y) = undefined

evalEither :: Expr -> Either String Int
evalEither (Val n) = return n
evalEither (Div x y) = undefined

errorS :: Show a => a -> a -> String
errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

type Prof = State Int

tickProf :: Prof ()
tickProf = do
  x <- S.get -- use get and put from the state monad
  S.put (x + 1)

evalProf :: Expr -> Prof Int
evalProf (Val n) = return n
evalProf (Div x y) = do
  m <- evalProf x
  n <- evalProf y
  tickProf
  return (m `div` n)

goProf :: Expr -> IO ()
goProf e = putStrLn $ "value: " ++ show x ++ ", count: " ++ show s
  where
    (x, s) = S.runState (evalProf e) 0 :: (Int, Int)

class Monad m => MonadError e m where
  throwError :: e -> m a

instance MonadError s (Either s) where
  throwError :: s -> Either s a
  throwError = undefined

class Monad m => MonadState s m where
  get :: m s
  put :: s -> m ()

tickStateInt :: MonadState Int m => m ()
tickStateInt = do
  (x :: Int) <- get
  put (x + 1)

instance MonadState s (State s) where
  get = S.get
  put = S.put

evalMega (Val n) = return n
evalMega (Div x y) = do
  n <- evalMega x
  m <- evalMega y
  if m == 0
    then throwError $ errorS n m
    else do
      tickStateInt
      return (n `div` m)

newtype Mega a = Mega {runMega :: Int -> Either String (a, Int)}

instance Monad Mega where
  return :: a -> Mega a
  return x = undefined
  (>>=) :: Mega a -> (a -> Mega b) -> Mega b
  ma >>= fmb = undefined

instance Applicative Mega where
  pure = return
  (<*>) = ap

instance Functor Mega where
  fmap = liftM

instance MonadError String Mega where
  throwError :: String -> Mega a
  throwError str = undefined

instance MonadState Int Mega where
  get = undefined
  put x = undefined

goMega :: Expr -> IO ()
goMega e = putStr $ pr (evalMega e)
  where
    pr :: Mega Int -> String
    pr f = case runMega f 0 of
      Left s -> "Raise: " ++ s ++ "\n"
      Right (v, cnt) ->
        "Count: " ++ show cnt ++ "\n"
          ++ "Result: "
          ++ show v
          ++ "\n"

newtype ExceptT e m a = MkExc {runExceptT :: m (Either e a)}

instance Monad m => Monad (ExceptT e m) where
  return :: forall a. a -> ExceptT e m a
  return x = MkExc (return (Right x) :: m (Either e a))

  (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
  p >>= f =
    MkExc $
      runExceptT p
        >>= ( \x -> case x of
                Left e -> return (Left e)
                Right a -> runExceptT (f a)
            )

instance Monad m => Applicative (ExceptT e m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (ExceptT e m) where
  fmap = liftM

instance Monad m => MonadError e (ExceptT e m) where
  throwError :: e -> ExceptT e m a
  throwError msg = MkExc (return (Left msg))

newtype StateT s m a = MkStateT {runStateT :: s -> m (a, s)}

instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return x = MkStateT $ \s -> return (x, s)

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  p >>= f = MkStateT $ \s -> do
    (r, s') <- runStateT p s
    runStateT (f r) s'

instance Monad m => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (StateT s m) where
  fmap = liftM

instance Monad m => MonadState s (StateT s m) where
  get :: StateT s m s
  get = MkStateT getIt
    where
      getIt :: s -> m (s, s)
      getIt s = undefined

  put :: s -> StateT s m ()
  put s = MkStateT putIt
    where
      putIt :: s -> m ((), s)
      putIt _ = undefined

class MonadTrans (t :: (* -> *) -> * -> *) where -- from Control.Monad.Trans (among other places)
  lift :: Monad m => m a -> t m a

instance MonadTrans (ExceptT e) where
  lift :: Monad m => m a -> ExceptT e m a
  -- Recall the type of MkExc
  -- MkExc :: m (Either e a) -> ExceptT e m a
  lift = MkExc . lift_
    where
      lift_ :: (Monad m) => m a -> m (Either e a)
      lift_ mt = Right <$> mt

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  -- Recall the type of MkStateT
  -- MkStateT  :: (s -> m (a,s)) -> StateT s m a
  lift ma = MkStateT $ \s -> do
    r <- ma
    return (r, s)

instance MonadError e m => MonadError e (StateT s m) where
  throwError :: e -> StateT s m a
  throwError = lift . throwError

instance MonadState s m => MonadState s (ExceptT e m) where
  get :: ExceptT e m s
  get = lift get

  put :: s -> ExceptT e m ()
  put = lift . put

evalExSt :: Expr -> StateT Int (Either String) Int
evalExSt = evalMega

evalStEx :: Expr -> ExceptT String Prof Int
evalStEx = evalMega

goExSt :: Expr -> IO ()
goExSt e = putStr $ pr (evalExSt e)
  where
    pr :: StateT Int (Either String) Int -> String
    pr f = case runStateT f 0 of
      Left s -> "Raise: " ++ s ++ "\n"
      Right (v, cnt) ->
        "Count: " ++ show cnt ++ "\n"
          ++ "Result: "
          ++ show v
          ++ "\n"

goStEx :: Expr -> IO ()
goStEx e = putStr $ pr (evalStEx e)
  where
    pr :: ExceptT String Prof Int -> String
    pr f = "Count: " ++ show cnt ++ "\n" ++ show r ++ "\n"
      where
        (r, cnt) = S.runState (runExceptT f) 0

newtype Id a = MkId a deriving (Show)

instance Monad Id where
  return x = undefined
  (MkId p) >>= f = undefined

instance Applicative Id where
  pure = return
  (<*>) = ap

instance Functor Id where
  fmap = liftM

type State2 s = StateT s Id -- isomorphic to State s

type Either2 s = ExceptT s Id -- isomorphic to Either s
