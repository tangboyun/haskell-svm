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
import qualified Data.Vector as V
import System.Random
import qualified Data.IntMap as M
import SVM.Types
import qualified SVM.Kernel.CPU as C
import Prelude hiding (zipWith)


main = $quickCheckAll

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
      vA = smoC cP cN y mQ
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
     
eps=1e-6 
     
data KKT = KKT Int Int Double Double 
           deriving (Show)
  
instance (UV.Unbox a,RealFloat a,Random a) => Arbitrary (DataSet a) where
  arbitrary = do
    nSample   <- choose (20,100)
    nVariable <- choose (10,200)
    nClass <- choose (2,20)
    ls <- vectorOf nSample $ elements [1..nClass] 
    ds <- vectorOf (nSample * nVariable) $ choose (-1.0,1.0)
    let lv = UV.fromList ls
        im = M.fromList $ zip [1..nClass] $ 
               map (\k->V.findIndices (== k) (UV.convert lv)) [1..nClass]
        dv = V.fromList $ map UV.fromList $ splitEvery nVariable ds
    return $ DataSet Nothing lv dv im
    where 
      splitEvery n [] = []
      splitEvery n xs = take n xs : (splitEvery n $ drop n xs)
    
instance Arbitrary KKT where
  arbitrary = do
    size <- choose (10,200)
    s <- arbitrary
    cP <- choose (2.0^^(-8),2.0^^8)
    cN <- choose (2.0^^(-8),2.0^^8)    
    return $ KKT s size cP cN
    
