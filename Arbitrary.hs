
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
module Arbitrary where
import Test.QuickCheck
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa hiding (map)
import qualified Data.Vector.Unboxed as UV
import System.Random
import SVM.Types
import qualified Data.Vector as V
import qualified Data.IntMap as M

data KKT = KKT Int Int Double Double 
           deriving (Show)
  
instance (UV.Unbox a,RealFloat a,Random a) => Arbitrary (DataSet a) where
  arbitrary = do
    nSample   <- choose (20,100)
    nVariable <- choose (10,500)
    nClass <- choose (2,nSample `div` 10)
    let cs = if nClass == 2
             then [1,-1]
             else [1..nClass]
    ls <- vectorOf nSample $ elements cs
    ds <- vectorOf (nSample * nVariable) $ choose (-1.0,1.0)
    let lv = UV.fromList ls
        im = M.fromList $ zip cs $ 
               map (\k->V.findIndices (== k) (UV.convert lv)) cs
        dv = V.fromList $ map UV.fromList $ splitEvery nVariable ds
    return $ DataSet Nothing lv dv im
    where 
      splitEvery n [] = []
      splitEvery n xs = take n xs : (splitEvery n $ drop n xs)
    
instance Arbitrary KKT where
  arbitrary = do
    size <- choose (200,400)
    s <- arbitrary
    cP <- choose (2.0^^(-8),2.0^^8)
    cN <- choose (2.0^^(-8),2.0^^8)    
    return $ KKT s size cP cN
     
