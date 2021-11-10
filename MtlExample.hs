{-
---
fulltitle: "MtlExample"
date: November 16, 2021
---

This file demonstrates the use of the `mtl` library
using the interpreter example from the [Transformers](Transformers.html) module.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-

-}

module MtlExample where

{-
The definitions of `StateT`, `ExceptT` and `Identity` come from separate modules
in the `mtl` library.
-}

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Identity
  ( Identity (runIdentity),
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
  )

data Expr
  = Val Int
  | Div Expr Expr
  deriving (Show)

-- | evaluates to 42
ok :: Expr
ok =
  (Val 1972 `Div` Val 2)
    `Div` Val 23

-- | divide by zero error
err :: Expr
err =
  Val 2
    `Div` ( Val 1
              `Div` (Val 2 `Div` Val 3)
          )

-- | nicely format the error
errorS :: Show a => a -> a -> String
errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

-- | increment the
tickStateInt :: MonadState Int m => m ()
tickStateInt = do
  (x :: Int) <- get
  put (x + 1)

{-

-}

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

{-

-}

goExSt :: Expr -> String
goExSt e = pr (eval e)
  where
    pr :: StateT Int (ExceptT String Identity) Int -> String
    pr f = case runIdentity (runExceptT (runStateT f 0)) of
      Left s -> "Raise: " ++ s
      Right (v, cnt) ->
        "Count: " ++ show cnt ++ "   "
          ++ "Result: "
          ++ show v

goStEx :: Expr -> String
goStEx e = pr (eval e)
  where
    pr :: ExceptT String (StateT Int Identity) Int -> String
    pr f = "Count: " ++ show cnt ++ "   " ++ pe r
      where
        (r, cnt) = runIdentity (runStateT (runExceptT f) 0)
    pe r = case r of
      Left s -> "Raise: " ++ s
      Right v -> "Result: " ++ show v

-- >>> goExSt ok

-- >>> goExSt err

-- >>> goStEx ok

-- >>> goStEx err
