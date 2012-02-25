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
module SVM.Resampling.CV 
       (
         cvSplit
       , cvSplit'
       )
       where

import           Control.Exception
import           Control.Monad.ST.Strict
import           Control.Parallel.Strategies
import qualified Data.IntMap                 as M
import           Data.List
import qualified Data.Vector                 as V
import           GHC.Conc
import           SVM.CSVM.Impl
import           SVM.Internal.Matrix
import           SVM.Resampling.CV.Impl
import           SVM.Resampling.Shuffle
import           SVM.Types
import           System.Random.MWC


{-# INLINE cvSplit' #-}
cvSplit' :: Int -> DataSet a -> CVFold
cvSplit' = (fst .) . (cvSplit $ runST $ create >>= save)

{-# INLINE cvSplit #-}
cvSplit :: Seed -> Int -> DataSet a -> (CVFold,Seed)
cvSplit !s !kFold !(DataSet _ _ _ idxS) = assert (kFold > 0) $
  let !ls = M.elems idxS
      !nClass = M.size idxS
      !(xs,s'') = foldl' (\(acc,seed) idxVec ->
                          let !l = V.length idxVec
                              !m = l `div` kFold
                              !(i_vec,s') = shuffle seed idxVec
                              !idxs = V.fromList $! 
                                      splitEvery m (kFold-1) i_vec
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
  in (CVFold ss,s'')
  where 
    splitEvery len count vec = go count vec []
      where 
        go !count !vec !acc | count > 0 = let (av,bv) = V.splitAt len vec
                                          in go (count-1) bv (av:acc)
                            | otherwise = vec : acc
                                       
