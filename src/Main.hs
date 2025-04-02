{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Control.Monad.State.Strict
  ( MonadIO (..),
    MonadState (get, put),
    StateT,
    evalStateT,
    gets,
  )
import Data.List (foldl', isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Eval
import Infer
import Parser
import Pretty
import Syntax
import Repline
  ( CompleterStyle (Prefix, Word),
    CompletionFunc,
    ExitDecision (Exit),
    HaskelineT,
    ReplOpts (ReplOpts, banner, command, finaliser, initialiser, multilineCommand, options, prefix, tabComplete),
    WordCompleter,
    abort,
    evalRepl,
    fileCompleter,
    wordCompleter,
  )
import System.Environment (getArgs)
import System.Exit (exitSuccess)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data IState = IState
  { tyctx :: TypeEnv, -- Type environment
    tmctx :: TermEnv -- Value environment
  }

initState :: IState
initState = IState emptyTyenv emptyTmenv

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: (Show e) => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

instance IsString (HaskelineT (StateT IState IO) String) where
  fromString :: String -> HaskelineT (StateT IState IO) String
  fromString = pure

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx'
  where
    (val, tmctx') = runEval env nm ex

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  mod <- hoistErr $ parseModule "<stdin>" source

  -- Type Inference ( returns Typing Environment )
  tyctx' <- hoistErr $ inferTop (tyctx st) mod

  -- Create the new environment
  let st' =
        st
          { tmctx = foldl' evalDef (tmctx st) mod,
            tyctx = tyctx' <> tyctx st
          }

  -- Update the interpreter state
  when update (put st')

  -- If a value is entered, print it.
  case lookup "it" mod of
    Nothing -> return ()
    Just ex -> do
      let (val, _) = runEval (tmctx st') "it" ex
      showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st = do
  case Infer.typeof (tyctx st) "it" of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: String -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ filter (not . ('#' `elem`)) $ ppenv (tyctx st)

-- :load command
load :: String -> Repl ()
load args = do
  contents <- liftIO $ L.readFile args
  exec True contents

-- :type command
typeof :: String -> Repl ()
typeof arg = do
  st <- get
  case Infer.typeof (tyctx st) arg of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> exec False (L.pack arg)

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-------------------------------------------------------------------------------
-- Tab completion
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: (MonadIO m) => [(String, CompletionFunc m)]
defaultMatcher =
  [ (":load", fileCompleter)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":browse", ":quit", ":type"]
  TypeEnv ctx <- gets tyctx
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

opts :: [(String, String -> Repl ())]
opts =
  [ ("load", load),
    ("browse", browse),
    ("quit", quit),
    ("type", Main.typeof)
  ]

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

-------------------------------------------------------------------------------
-- Shell
-------------------------------------------------------------------------------

shell :: Repl a -> IO ()
shell pre =
  flip evalStateT initState $
    evalRepl (pure ">>> ") cmd opts (Just ':') Nothing completer pre (pure Exit)

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell (return ())
    [fname] -> shell (load fname)
    ["test", fname] -> shell (load fname >> browse "" >> quit ())
    _ -> putStrLn "invalid arguments"
