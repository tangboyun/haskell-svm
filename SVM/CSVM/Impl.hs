{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
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
module SVM.CSVM.Impl 
       (
         trainOneImpl
       , trainOVOImpl
       , predictImpl
       , predictWithPreKernel
       , predictOneWithPreKernel
       )
       where
import           Control.Exception
import           Control.Parallel.Strategies
import           Data.Array.Repa             hiding (map)
import           Data.Array.Repa.Operators.Traversal
import qualified Data.IntMap                 as M
import           Data.List                   (sort,group,sortBy)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Generic as G
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
              new_y = UV.findIndices (\e -> e == i || e == j) y     
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

{-# INLINE predictImpl #-}
{-# SPECIALIZE predictImpl :: KernelPara -> Int -> Sample Double -> Sample Double -> [(Int,SVCoef)] -> Label #-}
{-# SPECIALIZE predictImpl :: KernelPara -> Int -> Sample Float -> Sample Float -> [(Int,SVCoef)] -> Label #-}
predictImpl :: (RealFloat a,UV.Unbox a) => KernelPara -> Int -> Sample a -> Sample a -> [(Int,SVCoef)] -> Label
predictImpl !kP !nClass !trainset !sample !xs =
    let f = kernel kP
        n = V.length sample
    in assert (UV.length (sample `atV` 0) ==
               UV.length (trainset `atV` 0)) $
     UV.fromList $! withStrategy 
     (parBuffer numCapabilities rdeepseq) $ map 
     (\idx_sample -> 
       let x = sample `atV` idx_sample
           final = head $ head $ 
                sortBy (\a b -> compare (length b) (length a)) $ 
                group $ sort $ withStrategy 
                (parBuffer numCapabilities rdeepseq) $ map
                (\(num,SVCoef r idxs sv_coef) ->
                  let (i,j) = decode nClass num
                      sum_dec = UV.ifoldl'            
                                (\acc idx coef ->
                                  let x1 = trainset `atV` 
                                            (idxs `atUV` idx)
                                      acc' = acc + coef * 
                                             (realToFrac (f x x1))
                                   in acc') 0.0 sv_coef
                      dec_v = sum_dec - r
                      vote = if dec_v > 0 
                             then i 
                             else j
                  in vote) xs
       in final) [0..n-1]
     
{-# INLINe predictWithPreKernel #-}
{-# SPECIALIZE predictWithPreKernel :: Int -> V.Vector Int -> Matrix Double -> [(Int,SVCoef)] -> Label #-}
{-# SPECIALIZE predictWithPreKernel :: Int -> V.Vector Int -> Matrix Float -> [(Int,SVCoef)] -> Label #-}
{-# SPECIALIZE predictWithPreKernel :: Int -> UV.Vector Int -> Matrix Double -> [(Int,SVCoef)] -> Label #-}
{-# SPECIALIZE predictWithPreKernel :: Int -> UV.Vector Int -> Matrix Float -> [(Int,SVCoef)] -> Label #-}
predictWithPreKernel :: (RealFloat a,UV.Unbox a,G.Vector v Int) => Int -> v Int -> Matrix a -> [(Int,SVCoef)] -> Label
predictWithPreKernel nClass testIdx mK xs =
    let !tes_idx = G.length testIdx
    in UV.fromList $! concat $ withStrategy 
       (parBuffer numCapabilities rdeepseq) $ map (map
       (\i -> 
         let !test_idx = testIdx `atG` i
             final = head $ head $ 
                     sortBy (\a b -> compare (length b) (length a)) $ 
                     group $ sort $ map
                     (\(num,SVCoef r idxs sv_coef) ->
                       let !(i,j) = decode nClass num
                           !sum_dec = UV.ifoldl'            
                                      (\acc idx coef ->
                                        let train_idx = idxs `atUV` idx
                                            acc' = acc + coef * 
                                                   (realToFrac 
                                                    (mK `atM` (Z:.train_idx:.test_idx)))
                                        in acc') 0.0 sv_coef
                           !dec_v = sum_dec - r
                           !vote = if dec_v > 0 
                                   then i 
                                   else j
                       in vote) xs
         in final)) $ splitEvery 50 [0..tes_idx-1]


{-# INLINe predictOneWithPreKernel #-}
{-# SPECIALIZE predictOneWithPreKernel :: Int -> Int -> Matrix Double -> [(Int,SVCoef)] -> Int #-}
{-# SPECIALIZE predictOneWithPreKernel :: Int -> Int -> Matrix Float -> [(Int,SVCoef)] -> Int #-}
predictOneWithPreKernel :: (RealFloat a,UV.Unbox a) => Int -> Int -> Matrix a -> [(Int,SVCoef)] -> Int
predictOneWithPreKernel nClass test_idx mK xs =
  let final = head $ head $ 
              sortBy (\a b -> compare (length b) (length a)) $ 
              group $ sort $ map
              (\(num,SVCoef r idxs sv_coef) ->
                let !(i,j) = decode nClass num
                    !sum_dec = UV.ifoldl'            
                               (\acc idx coef ->
                                 let train_idx = idxs `atUV` idx
                                     acc' = acc + coef * 
                                            (realToFrac 
                                             (mK `atM` (Z:.train_idx:.test_idx)))
                                 in acc') 0.0 sv_coef
                    !dec_v = sum_dec - r
                    !vote = if dec_v > 0 
                            then i 
                            else j
                in vote) xs
  in final
