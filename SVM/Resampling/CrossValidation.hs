{-# LANGUAGE BangPatterns #-}
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
module SVM.Resampling.CrossValidation 
       (
         cvSplit
       , cvSplit'
       )
       where

import System.Random.MWC
import qualified Data.Vector as V
import qualified Data.IntMap as M
import Data.List
import SVM.Resampling.Shuffle
import SVM.Types                 
import Control.Monad.ST.Strict  


{-# INLINE cvSplit' #-}
cvSplit' :: Int -> DataSet a -> [(V.Vector Int,V.Vector Int)]
cvSplit' = (fst .) . (cvSplit $ runST $ create >>= save)

{-# INLINE cvSplit #-}
cvSplit :: Seed -> Int -> DataSet a -> ([(V.Vector Int,V.Vector Int)],Seed)
cvSplit !s !kFold !(DataSet _ _ _ idxSlice) = 
  let !ls = M.elems idxSlice
      !nClass = M.size idxSlice
      !(xs,s'') = foldl' (\(acc,seed) idxVec ->
                          let !l = V.length idxVec
                              !m = ceiling $! fromIntegral l / fromIntegral kFold
                              !n = m * kFold
                              !(i_vec,s') = shuffle_v seed n 
                              !idxs = V.fromList $! 
                                      map (V.filter (< l)) $ splitEvery m i_vec
                          in (idxs:acc,s')) ([],s) ls
      !cvVec = V.fromList xs
      ss = map (\k -> 
                 let !testset = V.concatMap (\e -> e `V.unsafeIndex` k) cvVec
                     !trainset = V.concatMap 
                                 (\e -> V.foldl' 
                                        (\acc frag -> 
                                          acc V.++ frag
                                        ) V.empty $! 
                                        V.ifilter (\idx _ -> idx /= k) e) cvVec
                 in (trainset,testset)
               ) [0..kFold-1]
  in (ss,s'')
  where 
    splitEvery n vec = go vec []
      where 
        go vec acc | V.null vec = acc
                   | otherwise = let (av,bv) = V.splitAt n vec
                                 in go bv (av:acc)
