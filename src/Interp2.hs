{-# LANGUAGE TupleSections #-}
module Interp2 where

import Data.Bool (bool)
import Data.List (intercalate)
import Data.Maybe (isJust)

import Expr
import Normalize
import Value
import Env2

-- 解釈

interp :: Expr -> Env -> Com Value
interp expr env = delay $ case expr of
  Lam x e   -> return (VFun (\ val -> interp e ((x, val) : env)))
  App e v   -> join (fun_ <$> interp e env <*> return (interp v env))
  Var x     -> rho env x
  Con n     -> return (VNat n)
  Let bs e  -> interp e (modifyEnv bs env)
  Sub e1 e2 -> sub' <$> interp e1 env <*> interp e2 env
  If e1 e2 thn els
            -> ifge' <$> interp e1 env <*> interp e2 env <*> interp thn env <*> interp els env

-- 環境更新子

modifyEnv :: Heap -> Env -> Env
modifyEnv bnds env = env'
  where
    env' = foldr f env bnds
    f (name, exp) = ((name, interp exp env') :)

-- Value 上のプリミティブ演算

ifge' :: Value -> Value -> Value -> Value -> Value
ifge' (VNat m) (VNat n) thn els = bool els thn (m >= n)

sub' :: Value -> Value -> Value
sub' (VNat m) (VNat n) = VNat (m - n)

add' :: Value -> Value -> Value
add' (VNat m) (VNat n) = VNat (m + n)

div' :: Value -> Value -> Value
div' (VNat m) (VNat n) = VNat (m `div` n)

-- 近似列上のプリミティブ演算

bot :: Com Value
bot = interp (Let [("bot", Var "bot")] (Var "bot")) []

por :: Com Value -> Com Value -> Com Value
por x y = case (runIter x, runIter y) of
  (x',y') -> case x' of
    Left (VNat 0) -> case y' of
      Left (VNat 0) -> return (VNat 0)
      Left v       -> return v
      _            -> bot
    _ -> case y' of
      Left (VNat 0) -> delay (repack x')
      Left v        -> return v
      _             -> delay (por (repack y') (repack x'))

porAlt :: Com Value -> Com Value -> Com Value
porAlt x y = case (runIter x, runIter y) of
  (Left l, _) -> return l
  (_, Left r) -> return r
  (x', y')    -> delay $ porAlt (repack x') (repack y')

repack :: Either Value (Com Value) -> Com Value
repack = either return id

-- 値の近似列とその極限

computation :: Expr -> Com Value
computation = flip interp (modifyEnv heap0 []) . normalize

eval :: Expr -> Value
eval = limit . computation
