module Term where

import Numeric.Natural

data Term a = Var a
            | Con Natural
            | Lam a (Term a)
            | App (Term a) (Term a)
            | Let [TBnd a] (Term a)
            | Sub (Term a) (Term a)
            | If (Term a) (Term a) (Term a) (Term a)

type TBnd a = (a, Term a)
