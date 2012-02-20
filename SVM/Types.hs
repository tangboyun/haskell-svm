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

module SVM.Types 
       
       
       where
import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Text as T

data KernelPara = Linear 
                | Poly    {-# UNPACK #-} !Int
                          {-# UNPACK #-} !Double -- ^ gamma 
                          {-# UNPACK #-} !Double -- ^ coef0
                | RBF     {-# UNPACK #-} !Double -- ^ gamma
                | Sigmoid {-# UNPACK #-} !Double -- ^ gamma
                          {-# UNPACK #-} !Double -- ^ coef0
                  deriving (Eq)
                           
data DataSet a = DataSet {
    labelText :: !(Maybe (M.IntMap T.Text)) -- ^ Text label
   ,labels :: !(UV.Vector Int)              -- ^ classification categories
                                           -- ^ begin from 1 
   ,samples :: !(RealFloat a => (V.Vector (UV.Vector a)))
   ,idxSlice :: !(M.IntMap (V.Vector Int)) -- ^ Indexes for each class
  }

data SVMPara = SVMPara {
   kernelPara :: !KernelPara
  ,cost :: {-# UNPACK #-} !Double 
  ,weight :: !(M.IntMap Double)
--,probEst :: {-# UNPACK #-} !Bool  
  }

setGamma :: SVMPara -> Double -> SVMPara
setGamma svmPara gamma =
  case kernelPara svmPara of 
    Poly i g c  -> svmPara {kernelPara=Poly i gamma c}
    RBF  g      -> svmPara {kernelPara=RBF gamma}
    Sigmoid g c -> svmPara {kernelPara=Sigmoid gamma c}
    _           -> svmPara
    
setCost :: SVMPara -> Double -> SVMPara
setCost svmPara c = svmPara {cost=c}

setWeight :: SVMPara -> M.IntMap Double -> SVMPara
setWeight svmPara w = svmPara {weight=w}
