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

import Test.QuickCheck.All
import Test.QuickCheck
import SVM.Internal.SMO
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa hiding (map)
import qualified Data.Vector.Unboxed as UV
import System.Random
import SVM.Types
import qualified SVM.Kernel.CPU as C
import Prelude hiding (zipWith)
import Arbitrary


main = $quickCheckAll

eps=1e-6 

-- | sum(y^Talpha) = 0 && 
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
      vC = UV.imap (\idx e -> let c = if y `UV.unsafeIndex` idx == 1 then cP else cN     
                             in e >= 0 && e <= c + eps) vA
      vS = UV.zipWith (\label alpha -> fromIntegral label * alpha)
           y vA
  in UV.and vC && (eps > UV.sum vS)

prop_kernelMatrix_isSymmetric :: DataSet Float -> Bool
prop_kernelMatrix_isSymmetric dataSet =
  let mK = C.kernelFunc dataSet (RBF 1.0)
      mK' = transpose mK
  in foldAllP (&&) True $ zipWith (==) mK mK'
     
     
