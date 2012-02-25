{-# LANGUAGE BangPatterns 
            ,RankNTypes
            ,FlexibleContexts
 #-}
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

import           Control.Exception
import           Control.Parallel.Strategies
import           Data.Array.Repa             hiding (map)
import qualified Data.IntMap                 as M
import           Data.List                   (sort,group,sortBy)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import           GHC.Conc
import           SVM.CSVM.Impl
import           SVM.Internal.Matrix
import           SVM.Internal.Misc
import           SVM.Internal.SMO
import           SVM.Kernel.Function
import           SVM.Types

defaultCSVMPara = SVMPara (RBF 0) 1 M.empty OVO

buildCSVM :: (RealFloat a,UV.Unbox a,NFData a) => 
             (DataSet a -> KernelPara -> Matrix a) -> 
             DataSet a -> SVMPara -> SVM a
buildCSVM !f !dataSet !sp = 
  case kernelPara svmPara of
    RBF 0 -> let nFeature = UV.length $
                            samples dataSet `atV` 0
             in go $ RBF $! 1.0 / fromIntegral nFeature
    p     -> go p
  where 
    !k = M.size $ idxSlice dataSet 
    !svmPara = if M.null $ weight sp
               then if k == 2
                    then sp {weight=M.fromList [(1,1),(-1,1)]}
                    else sp {weight=M.fromList $ zip [1..k] $ repeat 1}
               else sp
    go !p' =
      let mK = f dataSet p'
      in SVM svmPara {kernelPara=p'} dataSet mK
           
{-# SPECIALIZE train :: SVM Double -> Model Double #-}
{-# SPECIALIZE train :: SVM Float -> Model Float #-}
train :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
train !svm = if (M.size . idxSlice . dataset $ svm) /= 2
             then case strategy $ para svm of
               OVO -> trainOVO svm
               OVA -> trainOVA svm
               DAG -> trainDAG svm
             else trainOne svm
                    
{-# INLINE trainOne #-}
{-# SPECIALIZE trainOne :: SVM Double -> Model Double #-}
{-# SPECIALIZE trainOne :: SVM Float -> Model Float #-}
trainOne :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
trainOne !svm@(SVM p dat mK) = 
  let !y = labels $ dat
      !yd = UV.map fromIntegral y
      !c = cost p
      !m = weight $! p
      !cP = c * (m M.! 1)
      !cN = c * (m M.! (-1))
      !mQ = computeUnboxedP $ unsafeTraverse mK id 
            (\f sh@(Z:.i:.j) ->
              (realToFrac $ yd `atUV` i * yd `atUV` j) * 
              (f sh))
      !coef = trainOneImpl cP cN y yd mQ      
  in Model svm $! M.fromList [(0,coef)]

     
            
{-# INLINE trainOVO #-}
{-# SPECIALIZE trainOVO :: SVM Double -> Model Double #-}
{-# SPECIALIZE trainOVO :: SVM Float -> Model Float #-}
trainOVO :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
trainOVO !svm@(SVM p dat mK_orign) = assert (M.size (idxSlice dat) > 2) $
  let !y = labels dat
      !m = weight $ p
      !c = cost p
      !nClass = M.size $! idxSlice dat
  in Model svm $! M.fromList $! trainOVOImpl p nClass y mK_orign

trainOVA :: SVM a -> Model a
trainOVA = undefined
                      
trainDAG :: SVM a -> Model a
trainDAG = undefined

{-# INLINE predict #-}
{-# SPECIALIZE predict :: Model Double -> Sample Double -> Label #-}
{-# SPECIALIZE predict :: Model Float -> Sample Float -> Label #-}
predict :: (RealFloat a,UV.Unbox a) => Model a -> Sample a -> Label
predict !(Model (SVM (SVMPara kP _ _ _) 
                 (DataSet _ _ dat sli) _) m) !sample = 
  let xs = M.assocs m
      nClass = M.size sli
  in predictImpl kP nClass dat sample xs
