{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- This file demonstrates the use of the `mtl` library
-- using the interpreter example from the Transformers module

module MtlExample where

import Control.Monad.Identity 
    ( Identity(runIdentity) )
import Control.Monad.State
    ( StateT(runStateT), MonadState(put, get) )
import Control.Monad.Except
    ( ExceptT, MonadError(throwError), runExceptT )

data Expr
  = Val Int
  | Div Expr Expr
  deriving (Show)

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

errorS :: Show a => a -> a -> String
errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

tickStateInt :: MonadState Int m => m ()
tickStateInt = do
  (x :: Int) <- get
  put (x + 1)

eval :: (MonadError String m, MonadState Int m) => Expr -> m Int
eval (Val n) = return n
eval (Div x y) = do
  n <- eval x
  m <- eval y
  if m == 0
    then throwError $ errorS n m
    else do
      tickStateInt
      return (n `div` m)

---------------------------------------------------------

-- newtype StateT s m a = MkStateT {runStateT :: s -> m (a, s)}
-- newtype ExceptT e m a = MkExc {runExceptT :: m (Either e a)}

-- StateT Int (ExceptT String Identity) Int 
-- Int -> (ExceptT String Identity) (Int, Int)
-- Int -> Identity (Either String (Int, Int))
-- Int -> Either String (Int, Int)

-- State s a = StateT s Identity a
-- Except e a = ExceptT e Identity a

goExSt :: Expr -> IO ()
goExSt e = putStr $ pr (eval e)
  where
    -- pr :: StateT Int (ExceptT String Identity) Int -> String
    pr f = case runIdentity (runExceptT (runStateT f 0)) of
      Left s -> "Raise: " ++ s ++ "\n"
      Right (v, cnt) ->
        "Count: " ++ show cnt ++ "\n"
          ++ "Result: "
          ++ show v
          ++ "\n"

-- ExceptT String (StateT Int Identity) Int
-- (StateT Int Identity) (Either String Int)
-- Int -> Identity (Either String Int, Int)
-- Int -> (Either String Int, Int)

goStEx :: Expr -> IO ()
goStEx e = putStr $ pr (eval e)
  where
    -- pr :: ExceptT String (StateT Int Identity) Int -> String
    pr f = "Count: " ++ show cnt ++ "\n" ++ show r ++ "\n"
      where
        (r, cnt) = runIdentity (runStateT (runExceptT f) 0)

