{-# LANGUAGE BangPatterns 
            ,RankNTypes
            ,FlexibleContexts
 #-}
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

import           Control.Exception
import           Control.Monad
import           Control.Monad.ST.Strict
import           Control.Parallel.Strategies
import           Data.Array.Repa             hiding (map)
import qualified Data.IntMap                 as M
import           Data.List                   (sort,group,sortBy)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import           GHC.Conc
import           SVM.Internal.Matrix
import           SVM.Internal.Misc
import           SVM.Internal.SMO
import           SVM.Kernel.Function
import           SVM.Types

defaultCSVMPara = SVMPara (RBF 0) 1 M.empty OVO

buildCSVM :: (RealFloat a,UV.Unbox a,NFData a) => 
             (DataSet a -> KernelPara -> Matrix a) -> 
             DataSet a -> SVMPara -> SVM a
buildCSVM !f !dataSet !sp = 
  case kernelPara svmPara of
    RBF 0 -> let nFeature = UV.length $
                            samples dataSet `atV` 0
             in go $ RBF $! 1.0 / fromIntegral nFeature
    p     -> go p
  where 
    !k = M.size $ idxSlice dataSet 
    !svmPara = if M.null $ weight sp
               then if k == 2
                    then sp {weight=M.fromList [(1,1),(-1,1)]}
                    else sp {weight=M.fromList $ zip [1..k] $ repeat 1}
               else sp
    go !p' =
      let mK = f dataSet p'
      in SVM svmPara {kernelPara=p'} dataSet mK
           
{-# SPECIALIZE train :: SVM Double -> Model Double #-}
{-# SPECIALIZE train :: SVM Float -> Model Float #-}
train :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
train !svm = if (M.size . idxSlice . dataset $ svm) /= 2
             then case strategy $ para svm of
               OVO -> trainOVO svm
               OVA -> trainOVA svm
               DAG -> trainDAG svm
             else trainOne svm
                    
{-# INLINE trainOne #-}
{-# SPECIALIZE trainOne :: SVM Double -> Model Double #-}
{-# SPECIALIZE trainOne :: SVM Float -> Model Float #-}
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
              (realToFrac $ yd `atUV` i * yd `atUV` j) * 
              (f sh))
      !(Si r vA) = smoC cP cN y mQ
      !coefs = UV.zipWith (*) yd vA
      !sv_idxs = UV.findIndices (/= 0.0) vA
      !sv_coef = UV.unsafeBackpermute coefs sv_idxs
  in Model svm $! M.fromList [(0,SVCoef r sv_idxs sv_coef)]

{-# INLINE encode #-}
encode :: Int -> Int -> Int -> Int 
encode !nClass !i !j = assert (j == -1 || i < j) $! nClass ^ (i - 1) + j
                    
{-# INLINE decode #-}
decode :: Int -> Int -> (Int,Int)
decode !nClass !num = assert (nClass > 1) $! 
                      if num == 0 
                      then (1,-1)
                      else num `divMod` nClass
            
{-# INLINE trainOVO #-}
{-# SPECIALIZE trainOVO :: SVM Double -> Model Double #-}
{-# SPECIALIZE trainOVO :: SVM Float -> Model Float #-}
trainOVO :: (RealFloat a,UV.Unbox a) => SVM a -> Model a
trainOVO !svm@(SVM p dat mK_orign) = assert (M.size (idxSlice dat) > 2) $
  let !idxs = idxSlice dat
      !y = labels dat
      !m = weight $ p
      !c = cost p
      !nClass = M.size idxs
      !ls = [1..nClass]
      ps = concat [[(i,j)|j<-ls,j>i]|i<-ls] 
  in Model svm $! M.fromList $! parMap rdeepseq 
        (\(i,j) ->
          let n = UV.length y
              mK = packKernel mK_orign $! UV.filter (\e -> e == i || e == j) y
              new_y = UV.findIndices (\idx -> idx == i || idx == j) y     
              y' = UV.map (\e -> if e == i then 1 else -1) new_y
              yd' = UV.map fromIntegral y'
              cP = c * (m M.! i)
              cN = c * (m M.! j)
              mQ = computeUnboxedS $ unsafeTraverse mK id 
                   (\f sh@(Z:.i:.j) ->
                     (realToFrac $ yd' `atUV` i * yd' `atUV` j) * 
                     (f sh))
              (Si r vA) = smoC cP cN y' mQ                   
              coefs = UV.zipWith (*) yd' vA
              sv_idxs = UV.findIndices (/= 0.0) vA
              sv_idxs_orig = UV.map (\e -> new_y `atUV` e) $ sv_idxs
              sv_coef = UV.unsafeBackpermute coefs sv_idxs
              num = encode nClass i j           
          in (num,SVCoef r sv_idxs_orig sv_coef)) ps
      

trainOVA :: SVM a -> Model a
trainOVA = undefined
                      
trainDAG :: SVM a -> Model a
trainDAG = undefined

{-# INLINE predict #-}
{-# SPECIALIZE predict :: Model Double -> Sample Double -> Label #-}
{-# SPECIALIZE predict :: Model Float -> Sample Float -> Label #-}
predict :: (RealFloat a,UV.Unbox a) => Model a -> Sample a -> Label
predict !(Model (SVM (SVMPara kP _ _ _) 
                 (DataSet _ _ dat sli) _) m) !sample = 
  let xs = M.assocs m
      f = kernel kP
      n = V.length sample
      nClass = M.size sli
  in assert (UV.length (sample `atV` 0) ==
             UV.length (dat `atV` 0)) $
     UV.fromList $! withStrategy 
     (parBuffer numCapabilities rdeepseq) $ map 
     (\idx_sample -> 
       let x = sample `atV` idx_sample
           final = head $ head $ 
                sortBy (\a b -> compare (length b) (length a)) $ 
                group $ sort $ withStrategy 
                (parBuffer numCapabilities rdeepseq) $ map
                (\(num,SVCoef r idxs sv_coef) ->
                  let (i,j) = decode nClass num
                      sum_dec = UV.ifoldl'            
                                (\acc idx coef ->
                                  let x1 = dat `atV` 
                                            (idxs `atUV` idx)
                                      acc' = acc + coef * 
                                             (realToFrac (f x x1))
                                   in acc') 0.0 sv_coef
                      dec_v = sum_dec - r
                      vote = if dec_v > 0 
                             then i 
                             else j
                  in vote) xs
       in final) [0..n-1]
            
