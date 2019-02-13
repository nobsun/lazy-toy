{-# LANGUAGE TupleSections #-}
module Normalize
  ( normalize
  , botExpr
  ) where

import Control.Monad.State
import Expr

type NameSupply = State Int

normalize :: Expr -> Expr
normalize = flip evalState 0 . norm

norm :: Expr -> NameSupply Expr
norm expr = case expr of
  v@(Var _)        -> return v
  c@(Con _)        -> return c
  Lam x body       -> Lam x <$> norm body
  App e v@(Var _)  -> App <$> norm e <*> return v
  App e1 e2        -> do
    { n <- get
    ; put (succ n)
    ; let v = "v_" ++ show n
    ; e1' <- norm e1
    ; e2' <- norm e2
    ; return (Let [(v, e2')] (App e1' (Var v)))
    }
  Let bs e         -> Let <$> mapM (\ (v, e') -> (v,) <$> norm e') bs <*> norm e
  Sub e1 e2        -> do
    { e1' <- norm e1
    ; e2' <- norm e2
    ; return (If e1' e2' (Sub e1' e2') botExpr)
    }
  If le re thn els -> If <$> norm le <*> norm re <*> norm thn <*> norm els

botExpr :: Expr
botExpr = Let [("⊥", Var "⊥")] (Var "⊥")
