module Env where

import Data.Bool

import Term
import Name
import Value

type Environment a = a -> Com Value
type Bind a = (a, Com Value)
type Env = Environment Name

rho0 :: Env
rho0 x = error ("Unbound Varable: " ++ x)

expandEnv :: Bind Name -> Env -> Env
expandEnv (x, c) rho y = bool (rho y) c (x == y)
