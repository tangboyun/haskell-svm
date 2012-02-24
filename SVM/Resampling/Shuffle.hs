{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module SVM.Resampling.Shuffle 
       
       where
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import System.Random.MWC
import Control.Monad.ST.Strict
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as MUV

shuffle_uv :: Seed -> Int -> (UV.Vector Int,Seed)
shuffle_uv !s !l = 
  let 
      n = l - 1
  in  runST $ do
        mv <- UV.unsafeThaw $ UV.enumFromN 0 l
        gen <- initialize $ fromSeed s
        forM_ [0..n] $ \idx -> do
          idx' <- uniformR (idx,n) gen
          val_i <- MUV.read mv idx
          val_j <- MUV.read mv idx'
          MUV.write mv idx val_j
          MUV.write mv idx' val_i
        s' <- save gen  
        per' <- UV.unsafeFreeze mv
        return $! (per',s')
      
shuffle_v :: Seed -> Int -> (V.Vector Int,Seed)
shuffle_v !s !l = 
  let 
      n = l - 1
  in  runST $ do
        mv <- V.unsafeThaw $ V.enumFromN 0 l
        gen <- initialize $ fromSeed s
        forM_ [0..n] $ \idx -> do
          idx' <- uniformR (idx,n) gen
          val_i <- MV.read mv idx
          val_j <- MV.read mv idx'
          MV.write mv idx val_j
          MV.write mv idx' val_i
        s' <- save gen  
        per' <- V.unsafeFreeze mv
        return $! (per',s')


