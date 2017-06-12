module Rand where

import Control.Monad.ST.Lazy
import Control.Monad
import System.Random.MWC

rList :: Variate a => Seed -> [a]
rList s = runST $ do
  g <- strictToLazyST $ restore s
  advance g

advance :: Variate a => Gen s -> ST s [a]
advance g = do
  x <- strictToLazyST $ uniform g
  xs <- x `seq` advance g
  return (x:xs)
