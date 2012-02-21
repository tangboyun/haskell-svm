{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
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
module SVM.CSVM 
       
       
       where

import           Control.DeepSeq
import           Data.Array.Repa
import qualified Data.IntMap         as M
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV
import           SVM.Internal.SMO
import           SVM.Internal.Matrix
import           SVM.Types

                  
data ClassSlice = CS {-# UNPACK #-} !Int
                     {-# UNPACK #-} !Int
                  

defaultCSVMPara = SVMPara (RBF 0) 1 M.empty

buildCSVM :: (RealFloat a,UV.Unbox a,NFData a) => 
             (DataSet a -> KernelPara -> Matrix a) -> 
             DataSet a -> SVMPara -> SVM a
buildCSVM f dataSet svmPara = 
  case kernelPara svmPara of
    RBF 0 -> let nFeature = UV.length
                            (V.unsafeIndex (samples dataSet) 0)
             in go $ RBF $! 1.0 / fromIntegral nFeature
    p     -> go p
  where 
    go p' =
        let mK = f dataSet p'
            y = UV.map fromIntegral $ labels dataSet
            at = UV.unsafeIndex    
            mQ = computeUnboxedP $ unsafeTraverse mK id 
                 (\f sh@(Z:.i:.j) ->
                    y `at` i * y `at` j * 
                   (f sh))
        in SVM svmPara{kernelPara=p'} dataSet mQ
           
trainCSVM :: Strategy -> SVM a -> Model a
trainCSVM s svm = if (M.size . idxSlice . dataset $ svm) == 2
                  then train2 svm
                  else case s of
                    OVO -> trainOVO svm
                    OVA -> trainOVA svm
                    
train2 :: SVM a -> Model a
train2 = undefined

trainOVO :: SVM a -> Model a
trainOVO = undefined

trainOVA :: SVM a -> Model a
trainOVA = undefined
                      
