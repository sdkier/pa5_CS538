{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Infer where

import Prelude hiding (foldr)

import Type
import Syntax

import Control.Monad (foldM, replicateM)
import Control.Monad.State
    ( MonadState(put, get), evalState, State )
import Control.Monad.Except
    ( MonadError(throwError), runExceptT, ExceptT )

import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
  deriving (Monoid, Semigroup)

newtype Unique = Unique { count :: Int }

type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe Type.Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

-- TODO-1: Implement the Substitutable instance for Type 
--         (all cases are done except TArray)
instance Substitutable Type where
  apply :: Subst -> Type -> Type
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TArrow` t2) = apply s t1 `TArrow` apply s t2
  apply s (TArray t) = TArray (apply s t)  -- TODO-1 DONE: apply a substitution to an array
  
  -- TODO-1: apply a substitution to an array
  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArrow` t2) = ftv t1 `Set.union` ftv t2
  ftv (TArray t) = ftv t  -- TODO-1 DONE: calculate free variables

  -- TODO-1: calculate the free variables of an array
instance Substitutable Scheme where
  apply :: Subst -> Scheme -> Scheme
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv :: Scheme -> Set.Set TVar
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply :: Substitutable a => Subst -> [a] -> [a]
  apply = fmap . apply
  ftv :: Substitutable a => [a] -> Set.Set TVar
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply :: Subst -> TypeEnv -> TypeEnv
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv :: TypeEnv -> Set.Set TVar
  ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (l `TArrow` r) (l' `TArrow` r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return nullSubst
unify (TArray t1) (TArray t2) = unify t1 t2  -- TODO-1 DONE: Unify the TArray type
unify t1 t2 = throwError $ UnificationFail t1 t2
-- TODO-1: Unify the TArray type

bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ Map.singleton a t

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Binop -> Type
ops Add = typeInt `TArrow` typeInt `TArrow` typeInt
ops Mul = typeInt `TArrow` typeInt `TArrow` typeInt
ops Sub = typeInt `TArrow` typeInt `TArrow` typeInt
ops Eql = typeInt `TArrow` typeInt `TArrow` typeBool

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

  Var x -> lookupEnv env x

  -- TODO-2: Handle the different pattern values of `x`
  --         Each has its own implications for typing
  Lam x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArrow` t1)

  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArrow t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TArrow` tv `TArrow` tv `TArrow` tv)

  Fix e1 -> do
    tv <- fresh
    inferPrim env [e1] ((tv `TArrow` tv) `TArrow` tv)

  -- TODO-1: Handle the Cons operator
  -- Suggestion: Separate this from the other ops because the constraint
  --             is more generic than the other ops

  -- TODO-2: Handle the Concat operator
  -- Suggestion: Separate this from the other ops because the constraint
  --             is more generic than the other ops

  Op op e1 e2 -> case op of 
    Cons -> do
      (s1, t1) <- infer env e1            -- Infer type of the element
      (s2, t2) <- infer (apply s1 env) e2 -- Infer type of the rest of the list
      s3 <- unify t2 (TArray (apply s2 t1))  -- Make sure t2 is an array of t1s
      return (s3 `compose` s2 `compose` s1, t2)
    _ -> inferPrim env [e1, e2] (ops op)  -- Handle other operators normally

  Lit (LInt _)  -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)
  Lit (LArray es) -> do
      case es of
        [] -> do
          tv <- fresh
          return (nullSubst, TArray tv)
        (e:rest) -> do
          (s1, t1) <- infer env e
          let inferElem (s, t) e = do
                (s', t') <- infer (apply s env) e
                s'' <- unify t t'
                return (s'' `compose` s' `compose` s, apply s'' t')
          (s2, elemType) <- foldM inferElem (s1, t1) rest
          -- First determine the element type
          let finalElemType = case e of
                Lit (LArray []) -> 
                  case rest of
                    [] -> TArray <$> fresh  -- Keep polymorphic if all empty
                    _  -> return elemType   -- Use type from non-empty lists
                _ -> return elemType
          elemT <- finalElemType
          return (s2, TArray elemT)

  -- TODO-1: Handle an Array literal
  -- Suggestion: Use foldM with a folding function that unifies 
  --             the result of infering on each element of the array

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer (apply s env) exp
    return (s' `compose` s, tf . TArrow t)

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = do
  ty <- inferExpr env ex
  inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TCon _)   = []
    fv (TArray t) = fv t    -- TODO-1 DONE: Handle TArray

    -- TODO-1: Handle TArray

    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
    normtype (TArray t) = TArray (normtype t)  -- TODO-1 DONE: Handle TArray
    -- TODO-1: Handle TArray
