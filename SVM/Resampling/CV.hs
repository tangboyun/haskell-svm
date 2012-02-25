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
         CVFold(..)
       , cvSplit
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
import Control.Exception
import Control.Parallel.Strategies
import GHC.Conc
import SVM.Internal.Matrix

newtype CVFold = CVFold [(V.Vector Int,V.Vector Int)]

-- kFoldCV_impl :: SVMPara -> DataSet a -> Matrix a -> CVFold -> Int
-- kFoldCV_impl p dat mK (CVFold xs) =
--   let !y = labels $ dat
--       !yd = UV.map fromIntegral y
--       !c = cost p
--       !m = weight $! p
--       !nClass = M.size $! idxSlice dat
--       !mM = if nClass == 2
--             then computeUnboxedP $ unsafeTraverse mK id 
--                  (\f sh@(Z:.i:.j) ->
--                    (realToFrac $ yd `atUV` i * yd `atUV` j) * 
--                    (f sh))
--             else mK
--   in sum $ withStrategy 
--      (parBuffer numCapabilities rdeepseq) $ 
--      map (\(trainIdx,testIdx) ->
--            let mM' = packKernel mM trainIdx
--                y_train = UV.unsafeBackpermute y 
--                          (UV.convert trainIdx)
--                yd_train = UV.unsafeBackpermute yd trainIdx
--                tQ' = packKernel2 mQ trainIdx testIdx
--                sis = if nClass == 2
--                      then [trainOneLowLevel 
--                            (c*(m M.! 1)) 
--                            (c*(m M.! -1)) 
--                            y_train 
--                            yd_train mM']
--                      else 
--                        let 
--                          cs = [1..nClass]
--                           withStrategy 
--                             (parBuffer numCapabilities rdeepseq) $ 
--                             map (\(i,j) ->
--                                   let cP = c*(m M.! i)
--                                       cN = c*(m M.! j)
--                                       y' = UV.unsafeBackpermute 
                                    
--                                     ) $
--                             concat [[(i,j)|j<-cs,j>i]|i<-cs]
--            in 
--            ) xs


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
                                       
