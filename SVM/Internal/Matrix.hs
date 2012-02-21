{-# LANGUAGE TypeOperators #-}
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

module SVM.Internal.Matrix 
       
       where

import           Data.Array.Repa
import qualified Data.Vector.Unboxed as UV
import Control.Exception (assert)    

type Matrix a = Array DIM2 a

{-# INLINE pack #-}
pack :: (Shape sh, UV.Unbox a) => Array U (sh :. Int) a -> UV.Vector Bool -> Array U (sh :. Int) a
pack source idxVec  = assert (lastDimLen == i) $ 
                      computeUnboxedP $ reshape (ss:. f idxVec) $ select f1 f2 ((size r) + 1)
  where
    r@(ss :. lastDimLen) = extent source
    i = UV.length idxVec
    {-# INLINE f #-}
    f = UV.foldl' (\acc bool -> if bool then acc + 1 else acc) 0
    {-# INLINE f1 #-}                                                                 
    f1 = \idx -> UV.unsafeIndex idxVec (idx `mod` size ss)
    {-# INLINE f2 #-}
    f2 = \idx -> unsafeIndex source (fromIndex r idx)
