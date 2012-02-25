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
       (
         shuffle
       )
       where
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.Random.MWC
import Control.Monad.ST.Strict
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as MUV

{-# INLINE shuffle #-}
{-# SPECIALIZE shuffle :: Seed -> UV.Vector Int -> (UV.Vector Int,Seed) #-}
{-# SPECIALIZE shuffle :: Seed -> V.Vector Int -> (V.Vector Int,Seed) #-}
{-# SPECIALIZE shuffle :: Seed -> V.Vector (UV.Vector Float) -> (V.Vector (UV.Vector Float),Seed) #-}
{-# SPECIALIZE shuffle :: Seed -> V.Vector (UV.Vector Double) -> (V.Vector (UV.Vector Double),Seed) #-}
shuffle :: G.Vector v a => Seed -> v a -> (v a,Seed)
shuffle !s !v =
  runST $ do
    let len = G.length v
        n   = len-1
    mv <- GM.new len
    gen <- initialize $ fromSeed s
    G.unsafeCopy mv v
    forM_ [0..n] $ \idx -> do
      idx' <- uniformR (idx,n) gen
      val_i <- GM.read mv idx
      val_j <- GM.read mv idx'
      GM.write mv idx val_j
      GM.write mv idx' val_i
    s' <- save gen
    v' <- G.unsafeFreeze mv
    return $! (v',s')
