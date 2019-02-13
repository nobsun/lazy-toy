module Value
  ( module Iter
  , module Identity
  , module Natural
  , Value (..)
  , Com
  , limit
  , approxSeries
  , showComputation
  ) where

import Data.Maybe

import Control.Monad.Trans.Iter as Iter
import Control.Monad.Identity as Identity hiding (foldM)
import Numeric.Natural as Natural

data Value = VNat  { val_ :: !Natural }
           | VFun  { fun_ :: Com Value -> Com Value }

instance Show Value where
  show (VNat n) = show n
  show (VFun _) = "<function>"

-- 近似列

type Com = Iter

limit :: Com Value -> Value
limit = runIdentity . retract

approxSeries :: Int -> Com Value -> [Maybe Value]
approxSeries n as
  | n <= 0    = Nothing : []
  | otherwise = case runIter as of
      Left v   -> Nothing : Just v : []
      Right iv -> Nothing : approxSeries (pred n) iv

-- 近似列の表示

showComputation :: Int -> Com Value -> String
showComputation n c = case break isJust (approxSeries n c) of
  (xs,[])  -> foldr (.) id (map showsValue xs) . ("..." ++) $ ""
  (xs,y:_) -> foldr (.) id (map showsValue xs) . showsValue y $ ""

showsValue :: Maybe Value -> ShowS
showsValue Nothing  = ("⊥," ++)
showsValue (Just v) = shows v
