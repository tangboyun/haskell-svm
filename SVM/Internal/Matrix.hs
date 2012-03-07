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

module SVM.Internal.Matrix 
       
       where

import           Control.Exception   (assert)
import           Data.Array.Repa     hiding (map)
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as UV
import           SVM.Internal.Misc

{-# INLINE packKernel #-}
{-# SPECIALIZE packKernel :: G.Vector v Int => Array U DIM2 Double -> v Int -> Array U DIM2 Double #-}
{-# SPECIALIZE packKernel :: G.Vector v Int => Array U DIM2 Float -> v Int -> Array U DIM2 Float #-}
{-# SPECIALIZE packKernel :: Array U DIM2 Double -> V.Vector Int -> Array U DIM2 Double #-}
{-# SPECIALIZE packKernel :: Array U DIM2 Double -> UV.Vector Int -> Array U DIM2 Double #-}
{-# SPECIALIZE packKernel :: Array U DIM2 Float -> V.Vector Int -> Array U DIM2 Float #-}
{-# SPECIALIZE packKernel :: Array U DIM2 Float -> UV.Vector Int -> Array U DIM2 Float #-}
packKernel :: (UV.Unbox a,G.Vector v Int) => Array U DIM2 a -> v Int -> Array U DIM2 a
packKernel source idxVec = assert (n < j) $! fromListUnboxed (Z:.n:.n) $! ls
  where
    (Z :. _ :. j) = extent source
    n = G.length idxVec
    ns = map (idxVec `atG`) [0..n-1]
    ls = map (\(x,y) -> 
               source `atM` (Z:.x:.j)) $ concat [[(x,y)|y<-ns]|x<-ns]
    
