module Interp where

import Data.Maybe

import Expr
import Normalize
import Value
import Env

interp :: Expr -> Env -> Com Value
interp expr ρ = case expr of
  Var x    -> ρ x
  Con n    -> return $ VNat n
  Lam x e  -> return $ VFun (\ com -> interp e (expandEnv (x, com) ρ))
  App e v  -> join $ (fun_ <$> interp e ρ) <*> return (interp v ρ)
  Let bs e -> interp e (modifyEnv bs ρ)
  Sub e e' -> sub' <$> interp e ρ <*> interp e' ρ
  If e e' thn els
           -> if' <$> interp e ρ <*> interp e' ρ <*> interp thn ρ<*> interp els ρ

modifyEnv :: Heap -> Env -> Env
modifyEnv h ρ = ρ'
  where
    ρ' = foldr modify ρ h
    modify (n, e) = expandEnv (n, interp e ρ') 

sub' :: Value -> Value -> Value
sub' (VNat m) (VNat n) = VNat (m - n)

if' :: Value -> Value -> Value -> Value -> Value
if' (VNat m) (VNat n) thn els = if m >= n then thn else els

eval :: Expr -> Value
eval = runIdentity . retract . flip interp rho0 . normalize

evalN :: Integer -> Expr -> Maybe Value
evalN n = runIdentity . retract . cutoff n . flip interp rho0 . normalize

approxSeries :: Expr -> [Maybe Value]
approxSeries e = map (flip evalN e) [0 ..]

showSeries :: Int -> [Maybe Value] -> String
showSeries n s = case break isJust s of
  (xs, [])  -> foldr f "..." xs
  (xs, y:_) -> foldr f (show (fromJust y)) xs
  where
    f _ zs = "⊥," ++ zs

--

bot :: Expr
bot = Let [("⊥",Var "⊥")] (Var "⊥")

three :: Expr
three = Lam "x" (Con 3)

three_1 :: Expr
three_1 = App three (Con 1)

three_bot :: Expr
three_bot = App three bot

ex1 :: Expr
ex1 = Sub three_bot (Sub (Con 7) (Con 5))
