module Expr
  ( module Term
  , module Name
  , Expr
  , Heap
  , heap0
  ) where

import Term
import Name

type Expr = Term Name
type Heap = [TBnd Name]

heap0 :: Heap
heap0 =  [("⊥",Var "⊥")]
