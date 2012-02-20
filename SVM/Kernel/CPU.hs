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
module SVM.Kernel.CPU 
       (
         kernelFunc
       )
       where
import           Control.Monad
import           Control.Monad.ST.Strict
import           Control.Parallel.Strategies
import           Data.Array.Repa             hiding ((++),map)
import           Data.List                   (sort)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import           GHC.Conc                    (numCapabilities)
import           SVM.Kernel.Function
import           SVM.Types

kernelFunc :: (RealFloat a,UV.Unbox a,NFData a) => 
              DataSet a -> KernelPara -> Array U DIM2 a
kernelFunc !dataSet !kernelPara =
  let !sps = samples dataSet
      !n = V.length sps
      !f = kernel kernelPara
      (ls1,ls2) = let !ss = [0..n-1] 
                  in (concat [[(x,y)|y<-ss,y>x]|x<-ss],[(x,x)|x<-ss])
      paraFunc = withStrategy 
                 (parBuffer numCapabilities rdeepseq) . map 
                 (\(i,j) -> 
                   let v_i = V.unsafeIndex sps i
                       v_j = V.unsafeIndex sps j
                       val = UV.sum $ 
                             UV.zipWith f v_i v_j
                   in  (i,j,val))
      vs1 = paraFunc ls1        
      vs2 = paraFunc ls2
      vec = UV.replicate (n*n) 0
      vec' = runST $ do
        mv <- UV.unsafeThaw vec
        forM_ vs2 $ \(i,j,val) ->
          MV.write mv (i*n + j) val
        UV.unsafeFreeze mv  
      vec'' = runST $ do    
        mv' <- UV.unsafeThaw vec'
        forM_ vs1 $ \(i,j,val) -> do
          MV.write mv' (i*n + j) val
          MV.write mv' (j*n + i) val
        UV.unsafeFreeze mv'  
  in fromUnboxed (Z:.n:.n) vec''

