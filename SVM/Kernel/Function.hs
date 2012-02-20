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
       , kernelLinear
       , kernelPoly
       , kernelRBF
       , kernelSigmoid
       )
       where

import SVM.Types
{-# INLINE kernel #-}
kernel :: RealFloat a => KernelPara -> a -> a -> a
kernel !kernelPara =
  case kernelPara of
    Linear      -> kernelLinear
    Poly d g c  -> kernelPoly d (realToFrac g) (realToFrac c)
    RBF g       -> kernelRBF (realToFrac g)
    Sigmoid g c -> kernelSigmoid (realToFrac g) (realToFrac c)


{-# INLINE kernelLinear #-}
kernelLinear :: RealFloat a => a -> a -> a
kernelLinear !x1 !x2 = x1 * x2

{-# INLINE kernelPoly #-}
kernelPoly :: RealFloat a => Int -> a -> a -> a -> a -> a
kernelPoly !degree !gamma !coef0 !x1 !x2 = (gamma*x1*x2+coef0)^^degree

{-# INLINE kernelRBF #-}
kernelRBF :: RealFloat a => a -> a -> a -> a
kernelRBF !gamma !x1 !x2 = exp $! -gamma*(x1 - x2)^^2

{-# INLINE kernelSigmoid #-}
kernelSigmoid :: RealFloat a => a -> a -> a -> a -> a
kernelSigmoid !gamma !coef0 !x1 !x2 = tanh $! gamma*x1*x2+coef0
