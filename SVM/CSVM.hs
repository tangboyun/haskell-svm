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
buildCSVM !f !dataSet !svmPara = 
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
           
train :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
train !svm = if (M.size . idxSlice . dataset $ svm) /= 2
              then case strategy $ para svm of
                OVO -> trainOVO svm
                OVA -> trainOVA svm
                DAG -> trainDAG svm
              else trainOne svm
                    
{-# INLINE trainOne #-}
trainOne :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
trainOne !svm = 
  let !mQ = matrixQ svm
      !y = labels $! dataset svm
      !yd = UV.map fromIntegral y
      !p = para $! svm
      !c = cost p
      !m = weight $! p
      !cP = c * (m M.! 1)
      !cN = c * (m M.! (-1))
      !(Si r vA) = smoC cP cN y mQ
      !sv_idxs = UV.findIndices (/= 0.0) $ UV.zipWith (*) yd vA
      !sv_coef = UV.unsafeBackpermute vA sv_idxs
  in Model svm $! M.fromList [(0,SVCoef r sv_idxs sv_coef)]
            

trainOVO :: SVM a -> Model a
trainOVO = undefined

trainOVA :: SVM a -> Model a
trainOVA = undefined
                      
trainDAG :: SVM a -> Model a
trainDAG = undefined
