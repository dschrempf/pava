{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , smooth
  ) where

-- TODO: Provide unsafe but faster functions.

-- TODO: LCM.

-- TODO: Write down assumptions of functions.

-- TODO: All this is slow:
--
-- - snoc is used extensively
--
-- - vectors are manipulated extensively
--
-- - functions recompute the length repeatedly
--
-- Solution: Use mutable vectors and store indices.

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Generic (Vector)

-- -- Differences between values in vector.
-- diff :: (Num a, Vector v a) => v a -> v a
-- diff v = V.zipWith (-) (V.tail v) v
-- {-# SPECIALIZE diff :: (Vector v Double) => v Double -> v Double #-}

-- TODO: THI TAKES 10 PER CENT OF THE TIME.
slope :: (Real a, Real b) => a -> a -> b -> b -> Double
slope x0 x1 y0 y1 = realToFrac (y1 - y0) / realToFrac (x1 - x0)
{-# SPECIALIZE slope :: Int -> Int -> Double -> Double -> Double #-}
{-# INLINE slope #-}

-- -- Slope at index.
-- slopeAt :: (Fractional a, Vector v a, Vector v Int) => Int -> v Int -> v a -> a
-- slopeAt i xs ys = slope (xs V.! (i-1)) (xs V.! i) (ys V.! (i-1)) (ys V.! i)
-- {-# SPECIALIZE slopeAt :: (Vector v Double, Vector v Int)
--                        => Int -> v Int -> v Double -> Double #-}

-- Slope at index.
slopeAt :: (Real a, Real b, Vector v a, Vector v b) => Int -> v a -> v b -> Double
slopeAt i xs ys = slope (xs V.! (i-1)) (xs V.! i) (ys V.! (i-1)) (ys V.! i)
{-# SPECIALIZE slopeAt :: (Vector v Double, Vector v Int) => Int -> v Int -> v Double -> Double #-}

-- TODO: THIS TAKES 25 PERCENT OF THE TIME.
rmSecondLast :: Vector v a => v a -> v a
rmSecondLast v = V.slice 0 (l-2) v `V.snoc` V.last v
  where l = V.length v
{-# SPECIALIZE rmSecondLast :: (Vector v Double) => v Double -> v Double #-}

-- Pool the last value in a vector until convexity is preserved.
pool :: (Real a, Ord a, Vector v a, Real b, Vector v b, Vector v Double)
     => v a -> v b -> v Double -> (v a, v b, v Double)
pool xs ys ss | l <= 2    = (xs, ys, ss)
              | otherwise = if s0 > s1 then (xs, ys, ss) else pool xs' ys' ss'
  where
    l  = V.length xs
    -- Slope of last element (length of ss is one lower than l).
    s0 = ss V.! (l-2)
    -- Slope of second last element (l >= 3, see guard above).
    s1 = ss V.! (l-3)
    xs' = rmSecondLast xs
    ys' = rmSecondLast ys
    ss' = V.init ss `V.snoc` slopeAt (l-2) xs' ys'
{-# SPECIALIZE pool :: (Vector v Double, Vector v Int)
                    => v Int -> v Double -> v Double -> (v Int, v Double, v Double) #-}

-- Check if vector is ordered strictly (<).
strictlyOrdered :: (Ord a, Vector v a, Vector v Bool) => v a -> Bool
strictlyOrdered xs | V.length xs <= 1 = True
                   | otherwise        = V.and $ V.zipWith (<) xs (V.tail xs)

-- | Greatest convex minorant. Uses the Pool Adjacent Violators Algorithm
-- (PAVA). The predictors are have to be ordered with no ties.
--
-- Usage:
--
-- @
--  gcm predictors responses = (indices, values, slopes)
-- @
gcm :: forall v a b . (Real a, Real b, Vector v a, Vector v b, Vector v Double, Vector v Int)
    => v a -> v b -> (v a, v b, v Double)
-- TODO: Check order of ps.
gcm ps rs | l == 0    = (V.empty, V.empty, V.empty)
          | l == 1    = (ps, rs, V.empty)
          | otherwise = go (V.take 1 ps, V.take 1 rs, V.singleton $ slopeAt 1 ps rs) (1 :: Int)
  where
    l  = V.length rs -- TODO: Error checking.
    -- xs and ys: x and y values of gcm
    -- i: next index of os
    go (xs, ys, ss) i | i >= l    = (xs, ys, ss)
                      | otherwise = go (pool xs' ys' ss') (i+1)
      where
        -- TODO: Especially here, mutable vectors should be used.
        xs' = xs `V.snoc` (ps V.! i)
        ys' = ys `V.snoc` (rs V.! i)
        ss' = ss `V.snoc` slopeAt (V.length xs' - 1) xs' ys'

-- | Fill in missing values of an indexed vector.
--
-- @
--  smooth [-2, 2, 4, 5] [0, 4, 10, 88] = [0, 1, 2, 3, 4, 7, 10, 88]
-- @
smooth :: (Vector v Bool, Vector v Double, Vector v Int) => v Int -> v Double -> v Double
smooth xs ys | l == 0             = V.empty
             | l == 1             = V.take 1 ys
             | strictlyOrdered xs =
               V.create (do zs <- M.new m
                            go zs 0 1 (bounds 1)
                            return zs)
             | otherwise  = error "smooth: Index vector is not strictly ordered."
  where
    a = V.head xs
    b = V.last xs
    m = b - a + 1
    l = if V.length xs == V.length ys
        then V.length xs
        else error "smooth: Index and value vector have different length."
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
