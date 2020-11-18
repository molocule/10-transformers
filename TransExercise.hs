{-# LANGUAGE FlexibleContexts #-}

module TransExercise where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), State, StateT, runState, runStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit hiding (State)
import WhileExn

type Store = Map Variable Value

evalE (Var x) = do
  m <- get
  case Map.lookup x m of
    Just v -> return v
    Nothing -> return (IntVal 0)
evalE (Val v) = return v
evalE (Op e1 o e2) = evalOp o <$> evalE e1 <*> evalE e2

evalOp :: Bop -> Value -> Value -> Value
evalOp Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalOp Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalOp Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalOp Divide (IntVal _) (IntVal 0) = IntVal 0
evalOp Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
evalOp Gt (IntVal i1) (IntVal i2) = BoolVal (i1 > i2)
evalOp Ge (IntVal i1) (IntVal i2) = BoolVal (i1 >= i2)
evalOp Lt (IntVal i1) (IntVal i2) = BoolVal (i1 < i2)
evalOp Le (IntVal i1) (IntVal i2) = BoolVal (i1 <= i2)
evalOp _ _ _ = IntVal 0

type M = ExceptT Value (State Store)

executeE :: Expression -> Store -> (Either Value Value, Store)
executeE e = runState (runExceptT comp)
  where
    comp :: M Value
    comp = evalE e

runE :: Expression -> IO ()
runE e = putStrLn $ display (fst (executeE e Map.empty))

-- >>> display (fst (executeE (Op  (Val (IntVal 1)) Divide (Val (IntVal 0))) Map.empty))
-- "Result: IntVal 0"

-- >>> display (fst (executeE (Op  (Val (IntVal 1)) Divide (Val (IntVal 1))) Map.empty))
-- "Result: IntVal 1"

raisesE :: Expression -> Value -> Test
s `raisesE` v = case executeE s Map.empty of
  (Left v', _) -> v ~?= v'
  _ -> TestCase $ assertFailure "Error in raises"

test_undefined :: Test
test_undefined =
  "undefined variable"
    ~: (Var "Y" `raisesE` IntVal 0)

test_divByZero :: Test
test_divByZero =
  "divide by zero"
    ~: (Op (Val (IntVal 1)) Divide (Val (IntVal 0)) `raisesE` IntVal 1)

test_badPlus :: Test
test_badPlus =
  "bad arg to plus"
    ~: Op (Val (IntVal 1)) Plus (Val (BoolVal True)) `raisesE` IntVal 2

test_expErrors :: Test
test_expErrors =
  "undefined variable & division by zero"
    ~: TestList [test_undefined, test_divByZero, test_badPlus]

-- >>> runTestTT test_expErrors
-- Counts {cases = 3, tried = 3, errors = 0, failures = 3}

evalS :: (MonadError Value m, MonadState Store m) => Statement -> m ()
evalS w@(While e (Block ss)) = do
  v <- evalE e
  case v of
    BoolVal True -> evalB (Block (ss ++ [w]))
    BoolVal False -> return ()
    IntVal _ -> return ()
evalS (Assign x e) = do
  v <- evalE e
  m <- get
  put (Map.insert x v m)
evalS (If e b1 b2) = do
  v <- evalE e
  case v of
    BoolVal True -> evalB b1
    BoolVal False -> evalB b2
    IntVal _ -> return ()
evalS (Try _ _ _) = error "evalS: unimplemented"
evalS (Throw _) = error "evalS: unimplemented"

evalB (Block ss) = mapM_ evalS ss

execute :: Block -> Store -> (Either Value (), Store)
execute b st = undefined

run :: Block -> IO ()
run block = do
  let (r, s) = execute block Map.empty
  putStrLn (display r)
  putStr "Output Store: "
  print s

-- >>> run $ Block [While (Val (IntVal 0)) (Block [])]
-- Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
--   undefined, called at /Users/sweirich/552/lecture-repos/10-transformers/TransExercise.hs:99:16 in fake_uid:TransExercise

raises :: Block -> Value -> Test
s `raises` v = case execute s Map.empty of
  (Left v', _) -> v ~?= v'
  _ -> TestCase $ assertFailure "Error in raises"

test_badWhile :: Test
test_badWhile = Block [While (Val (IntVal 0)) (Block [])] `raises` IntVal 3

test_badIf :: Test
test_badIf = Block [If (Val (IntVal 0)) (Block []) (Block [])] `raises` IntVal 4

-- >>> runTestTT test_badWhile
-- Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
--   undefined, called at /Users/sweirich/552/lecture-repos/10-transformers/TransExercise.hs:99:16 in fake_uid:TransExercise

-- >>> runTestTT test_badIf
-- Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
--   undefined, called at /Users/sweirich/552/lecture-repos/10-transformers/TransExercise.hs:99:16 in fake_uid:TransExercise

tryExpr :: Block
tryExpr = Block [Assign "x" (Val (IntVal 0)), Assign "y" (Val (IntVal 1)), Try (Block [If (Op (Var "x") Lt (Var "y")) (Block [Assign "a" (Val (IntVal 100)), Throw (Op (Var "x") Plus (Var "y")), Assign "b" (Val (IntVal 200))]) (Block [])]) "e" (Block [Assign "z" (Op (Var "e") Plus (Var "a"))])]

-- >>> run tryExpr
-- Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
--   undefined, called at /Users/sweirich/552/lecture-repos/10-transformers/TransExercise.hs:99:16 in fake_uid:TransExercise

-- display the result of evaluation
display :: Show a => Either Value a -> String
display (Left v) = "Uncaught exception: " ++ displayExn v
display (Right v) = "Result: " ++ show v

-- decode an exception value
displayExn :: Value -> String
displayExn (IntVal 0) = "Undefined variable"
displayExn (IntVal 1) = "Divide by zero"
displayExn (IntVal 2) = "Invalid arguments to operator"
displayExn (IntVal 3) = "Invalid condition in while statement"
displayExn (IntVal 4) = "Invalid condition in if statement"
displayExn v = "Error code: " ++ show v
