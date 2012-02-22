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

import           Control.Parallel.Strategies
import Control.Exception
import           Data.Array.Repa
import qualified Data.IntMap         as M
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import           SVM.Internal.SMO
import           SVM.Internal.Matrix
import           SVM.Types
import Control.Monad.ST.Strict
import Control.Monad
                  
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
        in SVM svmPara{kernelPara=p'} dataSet mK
           
train :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
train !svm = if (M.size . idxSlice . dataset $ svm) /= 2
             then case strategy $ para svm of
               OVO -> trainOVO svm
               OVA -> trainOVA svm
               DAG -> trainDAG svm
             else trainOne svm
                    
{-# INLINE trainOne #-}
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
              (realToFrac $ yd `at` i * yd `at` j) * 
              (f sh))
      !(Si r vA) = smoC cP cN y mQ
      !coefs = UV.zipWith (*) yd vA
      !sv_idxs = UV.findIndices (/= 0.0) vA
      !sv_coef = UV.unsafeBackpermute coefs sv_idxs
  in Model svm $! M.fromList [(0,SVCoef r sv_idxs sv_coef)]
     
{-# INLINE at #-}     
at :: UV.Unbox a => UV.Vector a -> Int -> a
at = UV.unsafeIndex    
            
encode :: Int -> Int -> Int -> Int 
encode !nClass !i !j = assert (i < j && j /= (-1)) $!
                    nClass ^ (i - 1) + j
                    
decode :: Int -> Int -> (Int,Int)
decode !nClass !num = num `divMod` nClass
            

trainOVO :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
trainOVO !svm@(SVM p dat mK_orign) = 
  let !idxs = idxSlice dat
      !y = labels dat
      !m = weight $ p
      !c = cost p
      !nClass = M.size idxs
      !ls = [1..nClass]
      ps = concat [[(i,j)|j<-ls,j>i]|i<-ls] 
  in Model svm $! M.fromList $ parMap rdeepseq 
        (\(i,j) ->
          let n = UV.length y
              bitset = UV.create $ do
                mv <- MV.new n
                forM_ [0..n-1] $ \idx ->
                  if y `at` idx == i ||
                     y `at` idx == j
                  then MV.write mv idx True
                  else MV.write mv idx False
                return mv       
              mK = computeUnboxedS $ 
                   pack (computeUnboxedS $ transpose (pack mK_orign bitset)) bitset
              new_y = UV.findIndices (\idx -> idx == i || idx == j) y     
              y' = UV.map (\e -> if e == i then 1 else -1) new_y
              yd' = UV.map fromIntegral y'
              cP = c * (m M.! i)
              cN = c * (m M.! j)
              mQ = computeUnboxedS $ unsafeTraverse mK id 
                   (\f sh@(Z:.i:.j) ->
                     (realToFrac $ yd' `at` i * yd' `at` j) * 
                     (f sh))
              (Si r vA) = smoC cP cN y' mQ                   
              coefs = UV.zipWith (*) yd' vA
              sv_idxs = UV.findIndices (/= 0.0) vA
              sv_coef = UV.unsafeBackpermute coefs sv_idxs
              num = encode nClass i j           
          in (num,SVCoef r sv_idxs sv_coef)) ps
      

trainOVA :: SVM a -> Model a
trainOVA = undefined
                      
trainDAG :: SVM a -> Model a
trainDAG = undefined
