{-# LANGUAGE TemplateHaskell #-}
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
module Main where

import           Arbitrary
import           Control.Monad.ST.Strict
import           Data.Array.Repa                      hiding (map)
import           Data.Array.Repa.Algorithms.Randomish
import qualified Data.IntMap                          as M
import qualified Data.List                            as L
import qualified Data.Vector                          as V
import qualified Data.Vector.Generic                  as G
import qualified Data.Vector.Unboxed                  as UV
import           Prelude                              hiding (zipWith)
import           SVM.CSVM
import           SVM.Internal.Misc
import           SVM.Internal.SMO
import qualified SVM.Kernel.CPU                       as C
import           SVM.Resampling.CV
import           SVM.Resampling.CV.Impl
import           SVM.Resampling.Shuffle
import           SVM.Types
import           System.Random
import           System.Random.MWC
import           Test.QuickCheck                      hiding (labels)
import           Test.QuickCheck.All

main = $quickCheckAll

eps=1e-6 
defaultSeed = runST $ create >>= save

-- | idx == sort $ shuffle idx 
prop_shuffle :: Int -> Bool
prop_shuffle len =
  let len' = len `mod` 2000
      s = toSeed $ UV.singleton $ fromIntegral len
      (vec,_) = shuffle s $ UV.enumFromN 0 len'
      ls = [0..len'-1]
  in ls == (L.sort $ map (vec `atUV`) ls)
                                            
-- | trainset + testset == whole dataset     
prop_cvSplit :: (Int,DataSet Float) -> Bool     
prop_cvSplit (i,dat) =
  let nSample = UV.length $ labels dat
      fold = 1 + (i `mod` (nSample `div` 10))
      CVFold xs   = cvSplit' fold dat
  in and $ L.map (\(a,b) ->
                   [0..nSample-1] == (L.sort $ V.toList $ a V.++ b)) xs
            
-- | sum(y^T * alpha) = 0 && forall i:
-- 0 <= alpha_i <= Cp for y_i = 1
-- 0 <= alpha_i <= Cn for y_i = -1
prop_smoC_KKT :: KKT -> Bool
prop_smoC_KKT (KKT s size cP cN) =
  let gen = mkStdGen s
      (g1,g2) = split gen
      mQ = randomishDoubleArray (Z:.size:.size) (-1000.0) 1000.0 s
      y = UV.fromList $ 
          map (\r -> if r > 0.5 then 1 else -1) $ 
          take size (randoms g1 :: [Double])
      Si _ vA = smoC cP cN y mQ
      vC = UV.imap (\idx e -> let c = if y `atUV` idx == 1 then cP else cN     
                             in e >= 0 && e <= c + eps) vA
      vS = UV.zipWith (\label alpha -> fromIntegral label * alpha)
           y vA
  in UV.and vC && (eps > UV.sum vS)

-- | K(i,j) == K(j,i)
prop_kernelFunc_kMatrixIsSymmetric :: DataSet Float -> Bool
prop_kernelFunc_kMatrixIsSymmetric dataSet =
  let mK = C.kernelFunc dataSet (RBF 1.0)
      mK' = transpose mK
  in foldAllS (&&) True $ zipWith (==) mK mK'
      
-- | model1 == train (shuffle dataset) 
--   model2 == train dataset
-- model1 `predict` testset == model2 `predict` testset
prop_trainANDpredict_shuffleDatasetWontChangePredictResult :: DataSet Float -> Bool
prop_trainANDpredict_shuffleDatasetWontChangePredictResult (DataSet lT ls ss is) =
  let nSample = UV.length ls
      n = nSample `div` 2
      (trainSample,testSample) = V.splitAt n ss
      is' = M.fromList $ map (\(key,v) -> (key,V.ifilter (\i _ -> i < n) v)) $ M.toList is
      ls' = UV.take n ls
      (idx_vec,_) = shuffle defaultSeed $ UV.enumFromN 0 n
      trainSet1 = DataSet lT ls' trainSample is'
      trainSet2 = DataSet lT (UV.unsafeBackpermute ls' idx_vec)
                  (V.unsafeBackpermute trainSample (V.convert idx_vec)) is'
      l1 = predict (train $ buildCSVM C.kernelFunc trainSet1 defaultCSVMPara) testSample
      l2 = predict (train $ buildCSVM C.kernelFunc trainSet2 defaultCSVMPara) testSample
  in l1 == l2
