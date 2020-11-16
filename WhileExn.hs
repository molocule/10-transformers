module WhileExn where

import Prelude hiding ((<>))

type Variable = String

newtype Block
  = Block [Statement] -- { s1; ... sn; }
  deriving (Eq, Show)

data Statement
  = Assign Variable Expression -- x = e;
  | If Expression Block Block -- if (e) { s1 } else { s2 }
  | While Expression Block -- while (e) { s }
  | Try Block Variable Block -- try { s1 } handle (x) { s2 }
  | Throw Expression -- throw e;
  deriving (Eq, Show)

data Expression
  = Var Variable -- x
  | Val Value -- v
  | Op Expression Bop Expression -- e1 op e2
  deriving (Eq, Show)

data Bop
  = Plus -- `+`  :: Int -> Int -> Int
  | Minus -- `-`  :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | Divide -- `/`  :: Int -> Int -> Int
  | Gt -- `>`  :: Int -> Int -> Bool
  | Ge -- `>=` :: Int -> Int -> Bool
  | Lt -- `<`  :: Int -> Int -> Bool
  | Le -- `<=` :: Int -> Int -> Bool
  deriving (Eq, Show, Enum)

data Value
  = IntVal Int
  | BoolVal Bool
  deriving (Eq, Show)
