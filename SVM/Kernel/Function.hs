{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module SVM.Kernel.Function 
       (
         kernel
       )
       where

import SVM.Types
import Data.Vector.Generic
import Prelude hiding (zipWith,sum,map)

{-# INLINE kernel #-}
kernel :: (RealFloat a,Vector v a) => KernelPara -> v a -> v a -> a
kernel !kernelPara =
  case kernelPara of
    Linear      -> kernelLinear
    Poly d g c  -> kernelPoly d (realToFrac g) (realToFrac c)
    RBF g       -> kernelRBF (realToFrac g)
    Sigmoid g c -> kernelSigmoid (realToFrac g) (realToFrac c)


{-# INLINE kernelLinear #-}
kernelLinear :: (RealFloat a,Vector v a) => v a -> v a -> a
kernelLinear !x1 !x2 = sum $ zipWith (*) x1 x2

{-# INLINE kernelPoly #-}
kernelPoly :: (RealFloat a,Vector v a) => Int -> a -> a -> v a -> v a -> a
kernelPoly !degree !gamma !coef0 !x1 !x2 = (gamma* (kernelLinear x1 x2)+coef0)^^degree

{-# INLINE kernelRBF #-}
kernelRBF :: (RealFloat a,Vector v a) => a -> v a -> v a -> a
kernelRBF !gamma !x1 !x2 = exp $! (-gamma) * (sum $ map (^^2) $ zipWith (-) x1 x2)

{-# INLINE kernelSigmoid #-}
kernelSigmoid :: (RealFloat a,Vector v a) => a -> a -> v a -> v a -> a
kernelSigmoid !gamma !coef0 !x1 !x2 = tanh $! gamma * (kernelLinear x1 x2) + coef0
