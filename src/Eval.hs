{-# LANGUAGE InstanceSigs #-}

module Eval (
  runEval,
  TermEnv,
  Value(..),
  emptyTmenv
) where

import Syntax

import Control.Monad.Identity ( Identity(runIdentity) )
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure [(Pattern, Expr)] TermEnv  -- Changed from (String, Expr, TermEnv)
  | VArray [Value]    -- TODO-1: Create a way to store arrays

  -- TODO-1: Create a way to store arrays `VArray`
  -- TODO-2: Edit VClosure to store a list of (pattern, expression) tuples
  --         these represent a map from pattern to expression body

type TermEnv = Map.Map String Value
type Interpreter t = Identity t

instance MonadFail Identity where
  fail :: String -> Identity a
  fail = error

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show :: Value -> String
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure{} = "<<closure>>"
  show (VArray vs) = show vs    -- TODO-1: Show VArray
  -- TODO-1: Show VArr

-- TODO-2: add a checkeq function to compare literals and values
checkeq :: Lit -> Value -> Bool
checkeq (LInt n) (VInt m) = n == m
checkeq (LBool b1) (VBool b2) = b1 == b2
checkeq _ _ = False

-- TODO-2: Add a match function to handle pattern matching
-- match :: [(Pattern, Expr)] -> Value -> (Expr, TermEnv)
-- When matching against a pattern, you can check:
-- 1. Is the pattern a PVar? -> always match, this is the generic case
-- 2. Is the pattern a PLit? -> match if the argument is equivalent to the literal
-- 3. Is the pattern a (x:xs) structure? -> match if the argument is a non-empty list
-- 4. Otherwise, check another pattern
match :: [(Pattern, Expr)] -> Value -> Maybe (Expr, TermEnv)
match [] _ = Nothing
match ((pat, expr):rest) val = case pat of
  PVar x -> Just (expr, Map.singleton x val)
  PLit lit -> if checkeq lit val 
              then Just (expr, Map.empty)
              else match rest val
  PCons p1 p2 -> case val of
    VArray (v:vs) -> case match [(p1, expr)] v of  -- Change undefined to expr
      Just (_, env1) -> case match [(p2, expr)] (VArray vs) of  -- Change undefined to expr
        Just (_, env2) -> Just (expr, env1 `Map.union` env2)
        Nothing -> match rest val
      Nothing -> match rest val
    _ -> match rest val


eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Lit (LInt k)  -> return $ VInt k
  Lit (LBool k) -> return $ VBool k
  Lit (LArray es) -> do    -- TODO-1: Handle evaluating arrays
    values <- mapM (eval env) es
    return $ VArray values

  -- TODO-1: Handle evaluating arrays
  -- Suggestion: Use a recursive call to evaluate each element one at a time

  Var x ->
    let Just v = Map.lookup x env in
    return v

  -- TODO-1: Add the Cons Operator
  -- Suggestion: Create a separate handler for this case
  --             because Cons is not the same type as other binop

  -- TODO-2: Add the Concat Operator
  -- Suggestion: Create a separate handler for this case
  --             because Cons is not the same type as other binop

  Op op a b -> case op of
    Cons -> do
      v1 <- eval env a
      VArray vs <- eval env b
      return $ VArray (v1:vs)
    Concat -> do
      VArray vs1 <- eval env a
      VArray vs2 <- eval env b
      return $ VArray (vs1 ++ vs2)
    _ -> do
      VInt a' <- eval env a
      VInt b' <- eval env b
      return $ binop op a' b'

  -- TODO-2: Change VClosure to store a list of patterns and expressions
  Lam x body ->
    return $ VClosure [(x, body)] env

  App fun arg -> do
    -- TODO-2: Implement pattern matching in App
    closure <- eval env fun
    argv <- eval env arg
    case closure of
      VClosure pats clo -> 
        case match pats argv of
          Just (body, matchEnv) -> eval (matchEnv `Map.union` clo) body
          Nothing -> error "Pattern match failure"
      _ -> error "Cannot apply non-function"

  Let x e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' env
    eval nenv body

  If cond tr fl -> do
    VBool br <- eval env cond
    if br
    then eval env tr
    else eval env fl

  Fix e -> do
    eval env (App e (Fix e))

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

-- TODO-2: Make sure that when you have a new definition for a function, you append the 
--         (pattern, body) to the environment instead of overwriting it
runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let res = runIdentity (eval env ex)
      newEnv = case (Map.lookup nm env, res) of
        (Just (VClosure pats e), VClosure newPats _) -> 
          Map.insert nm (VClosure (pats ++ newPats) e) env
        _ -> Map.insert nm res env
  in (res, newEnv)
