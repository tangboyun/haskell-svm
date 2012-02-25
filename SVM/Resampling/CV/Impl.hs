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

import SVM.CSVM.Impl
import SVM.Internal.Misc
import SVM.Internal.Matrix
import SVM.Types
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.IntMap as M
import Data.Array.Repa hiding (map)
import Control.Parallel.Strategies
import GHC.Conc

newtype CVFold = CVFold [(V.Vector Int,V.Vector Int)]

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
                         
               !yd_train = UV.unsafeBackpermute yd cVec
               !sis = if nClass == 2
                      then [(0,trainOneImpl 
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
