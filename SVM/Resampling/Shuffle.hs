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
import qualified Data.Vector.Unboxed as UV
import System.Random.MWC
import Control.Monad.ST.Strict
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as MUV

shuffle :: Seed -> Int -> (UV.Vector Int,Seed)
shuffle !s !l = 
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
      


