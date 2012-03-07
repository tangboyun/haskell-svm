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
module SVM.Resampling.CV.Impl
       (
         CVFold(..)
       , kFoldCV_impl
       )
       where

import           Control.Parallel.Strategies
import           Data.Array.Repa             hiding (map)
import qualified Data.IntMap                 as M
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import           GHC.Conc
import           SVM.CSVM.Impl 
import           SVM.Internal.Matrix
import           SVM.Internal.Misc
import           SVM.Types

newtype CVFold = CVFold [(V.Vector Int,V.Vector Int)]

{-# INLINE looCV_impl #-}
{-# SPECIALIZE looCV_impl :: Int -> Label -> Matrix Double -> SVMPara -> Int #-}
{-# SPECIALIZE looCV_impl :: Int -> Label -> Matrix Float -> SVMPara -> Int #-}
looCV_impl :: (RealFloat a,UV.Unbox a) => Int -> Label -> Matrix a -> SVMPara -> Int
looCV_impl !nClass !y !mK !p =
  let !yd = UV.map fromIntegral y
      !c = cost p
      !m = weight $! p
      !mM = if nClass == 2
            then computeUnboxedP $ unsafeTraverse mK id 
                 (\f sh@(Z:.i:.j) ->
                   (realToFrac $ yd `atUV` i * yd `atUV` j) * 
                   (f sh))
            else mK
      !l = UV.length y
  in sum $! withStrategy 
     (parBuffer numCapabilities rdeepseq) $ concat $
     map (map (\idx -> 
                let !trainIdx = UV.ifilter (\i _ -> i /= idx) $ UV.enumFromN 0 l
                    !mM' = packKernel mM trainIdx
                    !y_train = UV.ifilter (\i _ -> i /= idx) y
                    !sis = if nClass == 2
                           then let !yd_train = UV.ifilter (\i _ -> i /= idx) yd
                                in [(0,trainOneImpl 
                                       (c*(m M.! 1)) 
                                       (c*(m M.! (-1))) 
                                       y_train 
                                       yd_train mM')]
                           else trainOVOImpl p nClass y_train mM'
                    !l_pre = predictOneWithPreKernel nClass idx mK sis
                in if l_pre == y `atUV` idx then 0 else 1)) $
     splitEvery 20 [0..l-1]

{-# INLINE kFoldCV_impl #-}
{-# SPECIALIZE kFoldCV_impl :: Int -> Label -> CVFold -> Matrix Float -> SVMPara -> Int #-}
{-# SPECIALIZE kFoldCV_impl :: Int -> Label -> CVFold -> Matrix Double -> SVMPara -> Int #-}
kFoldCV_impl :: (RealFloat a,UV.Unbox a) => Int -> Label -> CVFold -> Matrix a -> SVMPara -> Int
kFoldCV_impl !nClass !y  !(CVFold xs) !mK !p =
  let !yd = UV.map fromIntegral y
      !c = cost p
      !m = weight $! p
      !mM = if nClass == 2
            then computeUnboxedP $ unsafeTraverse mK id 
                 (\f sh@(Z:.i:.j) ->
                   (realToFrac $ yd `atUV` i * yd `atUV` j) * 
                   (f sh))
            else mK
  in sum $ withStrategy 
     (parBuffer numCapabilities rdeepseq) $ 
     map (\(trainIdx,testIdx) ->
           let !cVec = UV.convert trainIdx
               !mM' = packKernel mM trainIdx
               !y_train = UV.unsafeBackpermute y cVec
               !sis = if nClass == 2
                      then let !yd_train = UV.unsafeBackpermute yd cVec
                           in [(0,trainOneImpl 
                               (c*(m M.! 1)) 
                               (c*(m M.! (-1))) 
                               y_train 
                               yd_train mM')]
                      else trainOVOImpl p nClass y_train mM'
               !l_pre = predictWithPreKernel nClass testIdx mK sis           
           in V.ifoldl' (\acc i e -> 
                          if (y `atUV` e) /= (l_pre `atUV` i)
                          then acc + 1
                          else acc) 0 testIdx
           ) xs
