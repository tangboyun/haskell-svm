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
import           SVM.Types

type Indexes = UV.Vector Int
type Coefs a = UV.Vector a
data SVCoef a = SVCoef !Indexes (Coefs a)
                  

data CSVM a = CSVM {
   para :: !SVMPara
  ,dataset :: !(DataSet a)
  ,matrixQ :: !(Array U DIM2 a)
  ,slice :: !(Maybe (V.Vector Int))
  ,packed :: !Bool
  ,svCoef :: !(Maybe (M.IntMap (SVCoef a)))
  }

defaultCSVMPara = SVMPara (RBF 0) 1 M.empty

buildCSVM :: (RealFloat a,UV.Unbox a,NFData a) => 
             (DataSet a -> KernelPara -> Array U DIM2 a) -> 
             DataSet a -> SVMPara -> CSVM a
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
        in CSVM svmPara{kernelPara=p'} dataSet mQ Nothing False Nothing
