{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# SPECIALIZE kernel :: Vector v Double => KernelPara -> v Double -> v Double -> Double #-}
{-# SPECIALIZE kernel :: Vector v Float => KernelPara -> v Float -> v Float -> Float #-}
kernel :: (RealFloat a,Vector v a) => KernelPara -> v a -> v a -> a
kernel !kernelPara =
  case kernelPara of
    Linear      -> kernelLinear
    Poly d g c  -> kernelPoly d (realToFrac g) (realToFrac c)
    RBF g       -> kernelRBF (realToFrac g)
    Sigmoid g c -> kernelSigmoid (realToFrac g) (realToFrac c)


{-# INLINE kernelLinear #-}
{-# SPECIALIZE kernelLinear :: Vector v Double => v Double -> v Double -> Double #-}    
{-# SPECIALIZE kernelLinear :: Vector v Float => v Float -> v Float -> Float #-}        
kernelLinear :: (RealFloat a,Vector v a) => v a -> v a -> a
kernelLinear !x1 !x2 = sum $ zipWith (*) x1 x2

{-# INLINE kernelPoly #-}
{-# SPECIALIZE kernelPoly :: Vector v Double => Int -> Double -> Double -> v Double -> v Double -> Double #-}
{-# SPECIALIZE kernelPoly :: Vector v Float => Int -> Float -> Float -> v Float -> v Float -> Float #-}
kernelPoly :: (RealFloat a,Vector v a) => Int -> a -> a -> v a -> v a -> a
kernelPoly !degree !gamma !coef0 !x1 !x2 = (gamma* (kernelLinear x1 x2)+coef0)^^degree

{-# INLINE kernelRBF #-}
{-# SPECIALIZE kernelRBF :: Vector v Double => Double -> v Double -> v Double -> Double #-}
{-# SPECIALIZE kernelRBF :: Vector v Float => Float -> v Float -> v Float -> Float #-}
kernelRBF :: (RealFloat a,Vector v a) => a -> v a -> v a -> a
kernelRBF !gamma !x1 !x2 = exp $! (-gamma) * (sum $ map (^^2) $ zipWith (-) x1 x2)

{-# INLINE kernelSigmoid #-}
{-# SPECIALIZE kernelSigmoid :: Vector v Double => Double -> Double -> v Double -> v Double -> Double #-}
{-# SPECIALIZE kernelSigmoid :: Vector v Float => Float -> Float -> v Float -> v Float -> Float #-}
kernelSigmoid :: (RealFloat a,Vector v a) => a -> a -> v a -> v a -> a
kernelSigmoid !gamma !coef0 !x1 !x2 = tanh $! gamma * (kernelLinear x1 x2) + coef0
