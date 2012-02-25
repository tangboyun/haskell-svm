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

module SVM.Internal.Misc 
       
       where

import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Array.Repa 


{-# INLINE atUV #-}     
atUV :: UV.Unbox a => UV.Vector a -> Int -> a
atUV = UV.unsafeIndex    
            
{-# INLINE atV #-}     
atV :: V.Vector a -> Int -> a
atV = V.unsafeIndex    

{-# INLINE atG #-}     
atG :: G.Vector v a => v a -> Int -> a
atG = G.unsafeIndex    

{-# INLINE atM #-}
atM :: (Repr r e,Shape sh) => Array r sh e -> sh -> e
atM = unsafeIndex
