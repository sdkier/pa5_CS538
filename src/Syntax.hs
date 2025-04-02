module Syntax where

type Var = String

-- TODO-2: Change lambdas to use `Pattern` ADT
data Expr
  = Var Var
  | App Expr Expr
  | Lam Var Expr
  | Let Var Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

-- TODO-1: Add an array literal `LArray`
-- This value should store elements of type `Expr`
data Lit
  = LInt Integer
  | LBool Bool
  | LArray [Expr]    -- Added for list support
  deriving (Show, Eq, Ord)

-- TODO-1: Add a `Cons` operator
-- TODO-2: Add a `Concat` operator
data Binop = Add | Sub | Mul | Eql | Cons  -- Added Cons operator
  deriving (Eq, Ord, Show)

-- TODO-2: Add a `Pattern` data type, capture the following cases:
-- let f x = ...      -- any identifier, use `PVar`, storing a `Var`
-- let f 0 = ...      -- any int literal, use `PLit`, storing a `Lit`
-- let f True = ...   -- any bool literal
-- let f [] = ...     -- empty list
-- let f (x:xs) = ... -- non empty list, use `PCons`, storing two `Patterns`
-- let f (x:[]) = ... -- list with one element
-- let f (x:y:xs) = ... -- list with at least two elements

type Decl = (String, Expr)

data Program = Program [Decl] Expr deriving (Show, Eq)
