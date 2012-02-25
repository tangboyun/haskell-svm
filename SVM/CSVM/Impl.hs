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
module SVM.CSVM.Impl 
       
       where
import           Control.Exception
--import           Control.Monad
--import           Control.Monad.ST.Strict
import           Control.Parallel.Strategies
import           Data.Array.Repa             hiding (map)
import qualified Data.IntMap                 as M
import           Data.List                   (sort,group,sortBy)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import           GHC.Conc
import           SVM.Internal.Matrix
import           SVM.Internal.Misc
import           SVM.Internal.SMO
import           SVM.Kernel.Function
import           SVM.Types

{-# INLINE encode #-}
encode :: Int -> Int -> Int -> Int 
encode !nClass !i !j = assert (j == -1 || i < j) $! nClass ^ (i - 1) + j
                    
{-# INLINE decode #-}
decode :: Int -> Int -> (Int,Int)
decode !nClass !num = assert (nClass > 1) $! 
                      if num == 0 
                      then (1,-1)
                      else num `divMod` nClass

{-# INLINE trainOneImpl #-}
{-# SPECIALIZE trainOneImpl :: Double -> Double -> Label -> UV.Vector Double -> Matrix Double -> SVCoef #-}
{-# SPECIALIZE trainOneImpl :: Double -> Double -> Label -> UV.Vector Double -> Matrix Float -> SVCoef #-}
trainOneImpl :: (RealFloat a,UV.Unbox a) => Double -> Double -> Label -> UV.Vector Double -> Matrix a -> SVCoef
trainOneImpl !cP !cN !y !yd !mQ =
  let !(Si r vA) = smoC cP cN y mQ
      !coefs = UV.zipWith (*) yd vA
      !sv_idxs = UV.findIndices (/= 0.0) vA
      !sv_coef = UV.unsafeBackpermute coefs sv_idxs
  in SVCoef r sv_idxs sv_coef

{-# INLINE trainOVOImpl #-}
{-# SPECIALIZE trainOVOImpl :: SVMPara -> Int -> Label -> Matrix Float -> [(Int,SVCoef)] #-}
{-# SPECIALIZE trainOVOImpl :: SVMPara -> Int -> Label -> Matrix Double -> [(Int,SVCoef)] #-}
trainOVOImpl :: (RealFloat a,UV.Unbox a) => SVMPara -> Int -> Label -> Matrix a -> [(Int,SVCoef)]
trainOVOImpl !p !nClass !y !mK_orign = assert (nClass > 2) $
  let !m = weight $ p
      !c = cost p
      !ls = [1..nClass]
      ps = concat [[(i,j)|j<-ls,j>i]|i<-ls] 
  in parMap rdeepseq 
        (\(i,j) ->
          let n = UV.length y
              mK = packKernel mK_orign $! UV.filter (\e -> e == i || e == j) y
              new_y = UV.findIndices (\idx -> idx == i || idx == j) y     
              y' = UV.map (\e -> if e == i then 1 else -1) new_y
              yd' = UV.map fromIntegral y'
              cP = c * (m M.! i)
              cN = c * (m M.! j)
              mQ = computeUnboxedS $ unsafeTraverse mK id 
                   (\f sh@(Z:.i:.j) ->
                     (realToFrac $ yd' `atUV` i * yd' `atUV` j) * 
                     (f sh))
              (Si r vA) = smoC cP cN y' mQ                   
              coefs = UV.zipWith (*) yd' vA
              sv_idxs = UV.findIndices (/= 0.0) vA
              sv_idxs_orig = UV.map (\e -> new_y `atUV` e) $ sv_idxs
              sv_coef = UV.unsafeBackpermute coefs sv_idxs
              num = encode nClass i j           
          in (num,SVCoef r sv_idxs_orig sv_coef)) ps

