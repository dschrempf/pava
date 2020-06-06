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

Possible sources:

- A [good introduction of PAVA](https://repository.tudelft.nl/islandora/object/uuid:5a111157-1a92-4176-9c8e-0b848feb7c30?collection=education).

- [Isotone regression in R](https://cran.r-project.org/web/packages/isotone/index.html).

- [Computation of greatest convex minorants in R](http://search.r-project.org/library/fdrtool/html/gcmlcm.html).

- [Wikipedia article on isotonic regression](https://en.wikipedia.org/wiki/Isotonic_regression).

-}

module Statistics.Gcm
  (
    gcm
  , smooth
  ) where

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

slope :: Fractional a => Int -> Int -> a -> a -> a
slope x0 x1 y0 y1 = (y1 - y0) / fromIntegral (x1 - x0)
{-# SPECIALIZE slope :: Int -> Int -> Double -> Double -> Double #-}
{-# INLINE slope #-}

-- -- Slope at index.
-- slopeAt :: (Fractional a, Vector v a, Vector v Int) => Int -> v Int -> v a -> a
-- slopeAt i xs ys = slope (xs V.! (i-1)) (xs V.! i) (ys V.! (i-1)) (ys V.! i)
-- {-# SPECIALIZE slopeAt :: (Vector v Double, Vector v Int)
--                        => Int -> v Int -> v Double -> Double #-}

-- Slope at index.
slopeAt :: (Fractional a, Vector v a, Vector v Int) => Int -> v Int -> v a -> a
slopeAt i xs ys = slope (xs V.! (i-1)) (xs V.! i) (ys V.! (i-1)) (ys V.! i)
{-# SPECIALIZE slopeAt :: (Vector v Double, Vector v Int) => Int -> v Int -> v Double -> Double #-}

rmSecondLast :: Vector v a => v a -> v a
rmSecondLast v = V.slice 0 (l-2) v `V.snoc` V.last v
  where l = V.length v
{-# SPECIALIZE rmSecondLast :: (Vector v Double) => v Double -> v Double #-}

-- Pool the last value in a vector until convexity is preserved.
pool :: (Fractional a, Ord a, Vector v a, Vector v Int) => v Int -> v a -> (v Int, v a)
pool xs ys | l <= 2    = (xs, ys)
           | otherwise = if s0 > s1 then (xs, ys) else pool (rmSecondLast xs) (rmSecondLast ys)
  where
    l  = V.length xs
    -- Slope of last element.
    s0 = slopeAt (l-1) xs ys
    -- Slope of second last element.
    s1 = slopeAt (l-2) xs ys
{-# SPECIALIZE pool :: (Vector v Double, Vector v Int)
                    => v Int -> v Double -> (v Int, v Double) #-}

-- | Greatest convex minorant of a list of observations. Uses the Pool Adjacent
-- Violators Algorithm (PAVA). The predictors are set to
--
-- \[
-- x_0 = 0, x_1 = 1, \ldots, x_{l-1} = l-1,
-- \]
--
-- where \(l\) is the length of the given observation vector. Hence, the
-- predictors are ordered and there are no ties, i.e.,
--
-- \[
-- x_0 < x_1 < ... < x_{l-1}.
-- \]
--
-- See https://en.wikipedia.org/wiki/Isotonic_regression, although this function
-- is antitonic in that the resulting vector is decreasing.
--
--
gcm :: forall v . (Vector v Double, Vector v Int) => v Double -> (v Int, v Double)
gcm os | l == 0    = (V.empty, V.empty)
       | otherwise = go (x0, V.take 1 os) (1 :: Int)
  where
    l  = V.length os
    x0 = V.singleton 0 :: v Int
    -- xs and ys: x and y values of gcm
    -- i: next index of os
    go (xs, ys) i | i >= l    = (xs, ys)
                  | otherwise = go (pool xs' ys') (i+1)
      where
        -- TODO: Especially here, mutable vectors should be used.
        xs' = xs `V.snoc` i
        ys' = ys `V.snoc` (os V.! i)

-- | Fill in missing values of an indexed vector.
--
-- @
--  smooth [-2, 2] [0, 4] = [0, 1, 2, 3, 4]
-- @
--
-- Assume that index vector is ordered.
smooth :: (Vector v Double, Vector v Int) => v Int -> v Double -> v Double
-- TODO: Check that lengths are equal?
smooth xs ys | l == 0    = V.empty
             | l == 1    = V.take 1 ys
             | otherwise = V.create
                           (do
                               zs <- M.new m
                               go zs 0 1 (bounds 1)
                               return zs)
  where
    a = V.head xs
    b = V.last xs
    m = b - a + 1
    l = V.length xs
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
