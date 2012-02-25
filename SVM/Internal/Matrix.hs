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

import           Data.Array.Repa hiding (map)
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Control.Exception (assert)    
import SVM.Internal.Misc

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
    
{-# INLINE packKernel2 #-}
{-# SPECIALIZE packKernel2 :: G.Vector v Int => Array U DIM2 Double -> v Int -> v Int -> Array U DIM2 Double #-}
{-# SPECIALIZE packKernel2 :: G.Vector v Int => Array U DIM2 Float -> v Int -> v Int -> Array U DIM2 Float #-}
{-# SPECIALIZE packKernel2 :: Array U DIM2 Double -> V.Vector Int -> V.Vector Int -> Array U DIM2 Double #-}
{-# SPECIALIZE packKernel2 :: Array U DIM2 Double -> UV.Vector Int -> UV.Vector Int -> Array U DIM2 Double #-}
{-# SPECIALIZE packKernel2 :: Array U DIM2 Float -> V.Vector Int -> V.Vector Int -> Array U DIM2 Float #-}
{-# SPECIALIZE packKernel2 :: Array U DIM2 Float -> UV.Vector Int -> UV.Vector Int -> Array U DIM2 Float #-}
packKernel2 :: (UV.Unbox a,G.Vector v Int) => Array U DIM2 a -> v Int -> v Int -> Array U DIM2 a
packKernel2 source train_idx test_idx = assert (j < max (G.length train_idx) (G.length test_idx)) $!
                                        fromListUnboxed (Z:.train_len:.test_len) $! ls
  where
    (Z :. _ :. j) = extent source
    train_len = G.length train_idx - 1
    test_len = G.length test_idx - 1
    trainIdxs = map (train_idx `atG`) [0..train_len]
    testIdxs = map (test_idx `atG`) [0..test_len]
    ls = map (\(train_i,test_j) ->
               source `atM` (Z:.train_i:.test_j)) $
         concat [[(a,b)|a<-trainIdxs]|b<-testIdxs]
