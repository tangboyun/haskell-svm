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
-- 以vector实现一个上三角阵
--
-----------------------------------------------------------------------------

module KMatrix where

import qualified Data.Vector.Unboxed as UV
import Data.List

newtype KMatrix a = K (UV.Vector a)
data Ix = I {-# UNPACK #-}!Int
            {-# UNPACK #-}!Int
          deriving (Eq)
                   
fromLists :: UV.Unbox a => [[a]] -> KMatrix a
fromLists xxs = K $ UV.fromList $ foldr1 (++) $
                    map (\(i,xs) -> drop i xs) $ zip [0..] xxs


idxPairs :: Int -> [Ix]
idxPairs n = concatMap (\(i,xs) -> drop i xs) $
             zip [0..] $
             splitEvery n [ I x y | x <- [0..n-1], y <- [0..n-1]]
  where 
    splitEvery :: Int -> [a] -> [[a]]
    splitEvery _ [] = []
    splitEvery n xs = take n xs : splitEvery n (drop n xs)

at :: UV.Unbox a => KMatrix a -> Ix -> a
at !(K m) !(I i j) = m `UV.unsafeIndex` (calc i j) -- i row, j col
  where
    n = floor $ sqrt $ fromIntegral $ 2 * (UV.length m)
    calc i j | i > j = calc j i
             | otherwise = i * n + j - i * (i + 1) `div` 2
{-# INLINE at #-}

instance Show Ix where
  show (I i j) = show (i,j)
