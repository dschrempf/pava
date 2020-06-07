{-# LANGUAGE FlexibleContexts #-}

{- |
Module      :  Statistics.Gcm
Description :  Compute greatest convex minorants
Copyright   :  (c) Dominik Schrempf, 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sat Jun  6 16:36:32 2020.

For general help, please refer to the README distributed with the library.

-}

module Statistics.Gcm
  (
    gcm
  , unsafeGcm
  , smooth
  , unsafeSmooth
  ) where

-- TODO: LCM.

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Generic (Vector)

-- -- Differences between values in vector.
-- diff :: (Num a, Vector v a) => v a -> v a
-- diff v = V.zipWith (-) (V.tail v) v
-- {-# SPECIALIZE diff :: (Vector v Double) => v Double -> v Double #-}

slope :: (Real a, Real b) => a -> a -> b -> b -> Double
slope x0 x1 y0 y1 = realToFrac (y1 - y0) / realToFrac (x1 - x0)
{-# SPECIALIZE slope :: Int -> Int -> Double -> Double -> Double #-}
{-# SPECIALIZE slope :: Double -> Double -> Double -> Double -> Double #-}
{-# INLINE slope #-}

-- Pool the last value in a vector until convexity is preserved.
pool :: (Ord a, Real a, Show a, Real b, Show b)
     => [a] -> [b] -> [Double] -> a -> b -> ([a], [b], [Double])
-- Points are ordered x0, x1, x2; and y0, y1, y2; s01 is slope from x0 to x1,
-- and so on.
pool xs@(x1:_) ys@(y1:_) ss@(s01:_) x2 y2 =
  if s01 < s12
  then (x2:xs, y2:ys, s12:ss)
  else pool (tail xs) (tail ys) (tail ss) x2 y2
  where
    s12 = slope x1 x2 y1 y2
-- Initialization and fallback if all points are removed during pooling.
pool [x] [y] [] x' y' = (x':[x], y':[y], [slope x x' y y'])
pool xs ys ss x y = error $ "pool: xs, ys, ss, x, y: "
                    ++ show xs ++ ", " ++ show ys ++ ", " ++ show ss
                    ++ ", " ++ show x ++ ", " ++ show y ++ "."
{-# SPECIALIZE pool :: [Int] -> [Double] -> [Double] -> Int -> Double
                    -> ([Int], [Double], [Double]) #-}
{-# SPECIALIZE pool :: [Double] -> [Double] -> [Double] -> Double -> Double
                    -> ([Double], [Double], [Double]) #-}

-- Check if vector is ordered strictly (<).
strictlyOrdered :: (Ord a, Vector v a, Vector v Bool) => v a -> Bool
strictlyOrdered xs | V.length xs <= 1 = True
                   | otherwise        = V.and $ V.zipWith (<) xs (V.tail xs)

-- | Greatest convex minorant. Uses the Pool Adjacent Violators Algorithm
-- (PAVA). It is required that the predictors are ordered with no ties, and that
-- the lengths of the vectors are equal.
--
-- Usage:
--
-- @
--  gcm predictors responses = (indices, values, slopes)
-- @
--
gcm :: (Real a, Real b, Show a, Vector v a, Show b, Vector v b, Vector v Bool)
    => v a -> v b -> ([a], [b], [Double])
gcm ps rs | lPs /= lRs = error $ "gcm: Number of predictors is " ++ show lPs
                         ++ ", but number of responses is " ++ show lRs ++ "."
          | not (strictlyOrdered ps) = error "gcm: The predictors are not strictly ordered."
          | otherwise = unsafeGcm ps rs
  where lPs = V.length ps
        lRs = V.length rs

-- | See 'gcm'.
--
-- Assume that:
-- - the lengths of the provided vectors are equal;
-- - the predictors are ordered.
unsafeGcm :: (Real a, Real b, Show a, Vector v a, Show b, Vector v b)
    => v a -> v b -> ([a], [b], [Double])
unsafeGcm ps rs | l == 0    = ([], [], [])
                | l == 1    = start
                | otherwise = go start (1 :: Int)
  where
    l     = V.length rs
    start = ([V.head ps], [V.head rs], [])
    -- xs and ys: x and y values of gcm
    -- i: next index of rs
    go (xs, ys, ss) i | i >= l    = (xs, ys, ss)
                      | otherwise = go (pool xs ys ss x' y') (i+1)
      where
        x'  = ps V.! i
        y'  = rs V.! i

-- | Fill in missing values of an indexed vector.
--
-- @
--  smooth [-2, 2, 4, 5] [0, 4, 10, 88] = [0, 1, 2, 3, 4, 7, 10, 88]
-- @
smooth :: (Vector v Bool, Vector v Double, Vector v Int) => v Int -> v Double -> v Double
smooth xs ys | V.length xs /= V.length ys =
                 error "smooth: Index and value vector have different length."
             | not (strictlyOrdered xs) =
                 error "smooth: Index vector is not strictly ordered."
             | otherwise = unsafeSmooth xs ys

-- | See 'smooth'.
--
--
-- Assume that:
-- - the lengths of the provided vectors are equal;
-- - the predictors are ordered.
unsafeSmooth :: (Vector v Bool, Vector v Double, Vector v Int) => v Int -> v Double -> v Double
unsafeSmooth xs ys | l == 0    = V.empty
                   | l == 1    = V.take 1 ys
                   | otherwise = V.create (do zs <- M.new m
                                              go zs 0 1 (bounds 1)
                                              return zs)
  where
    l = V.length xs
    a = V.head xs
    b = V.last xs
    m = b - a + 1
    -- 0 <= i < m; index traversing resulting vector
    -- 0 <= j < l; index traversing given vectors
    bounds i = (xs V.! (i-1), xs V.! i, ys V.! (i-1), ys V.! i)
    go zs i j (il, ir, yl, yr)
      | i   >= m  = return ()
      | a+i >= ir = do M.write zs i yr
                       go zs (i+1) (j+1) (bounds $ j+1)
      | otherwise = do M.write zs i (yl + dy)
                       go zs (i+1) j (il, ir, yl, yr)
          where dx = a + i - il
                dy = fromIntegral dx * slope il ir yl yr
