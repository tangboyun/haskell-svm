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

{-# SPECIALIZE kernelFunc :: DataSet Double -> KernelPara -> Matrix Double #-}
{-# SPECIALIZE kernelFunc :: DataSet Float -> KernelPara -> Matrix Float #-}
{-# INLINE kernelFunc #-}
kernelFunc :: (RealFloat a,UV.Unbox a,NFData a) => 
              DataSet a -> KernelPara -> Matrix a
kernelFunc !dataSet !kernelPara =
  let !sps = samples dataSet
      !n = V.length sps
      !l = UV.length $! V.unsafeIndex sps 0
      splitEvery n [] = []
      splitEvery n xs = take n xs : (splitEvery n $ drop n xs)
      !f = kernel kernelPara
      (ls1,ls2) = let !ss = [0..n-1] 
                  in (concat [[(x,y)|y<-ss,y>x]|x<-ss],[(x,x)|x<-ss])
      paraFunc = withStrategy 
                 (parBuffer numCapabilities rdeepseq) . (map (map 
                 (\(i,j) -> 
                   let v_i = V.unsafeIndex sps i
                       v_j = V.unsafeIndex sps j
                       val = f v_i v_j
                   in  (i,j,val))). splitEvery (ceiling $ 2000.0 / fromIntegral l))
      vs1 = concat $ paraFunc ls1        
      vs2 = concat $ paraFunc ls2
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
     
