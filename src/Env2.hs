module Env2 where

import Data.Bool
import Data.Maybe

import Term
import Name
import Value

type Bind' a = (a, Com Value)
type Env' a = [Bind' a]

type Bind = Bind' Name
type Env  = Env' Name

type Rho   = Name -> Com Value

rho0 :: Name -> Com Value
rho0 x = error ("Unbound Varable: " ++ x)

rho :: Env -> Rho
rho = foldr expandRho rho0

expandRho :: Bind -> Rho -> Rho
expandRho (var, val) rho var' = bool (rho var') val (var == var')
