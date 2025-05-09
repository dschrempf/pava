{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Statistics.Gcm
-- Description :  Compute greatest convex minorants
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jun  6 16:36:32 2020.
--
-- For general help, please refer to the README distributed with the library.
module Statistics.Gcm
  ( gcm,
    unsafeGcm,
  )
where

import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import Statistics.Pava.Common

-- Pool the last value in a vector until convexity is preserved.
pool ::
  (Ord a, Real a, Show a, Real b, Show b) =>
  [a] ->
  [b] ->
  [Double] ->
  a ->
  b ->
  ([a], [b], [Double])
-- Points are ordered x0, x1, x2; and y0, y1, y2; s01 is slope from x0 to x1,
-- and so on.
pool xs@(x1 : _) ys@(y1 : _) ss@(s01 : _) x2 y2 =
  if s01 < s12
    then (x2 : xs, y2 : ys, s12 : ss)
    else pool (drop 1 xs) (drop 1 ys) (drop 1 ss) x2 y2
  where
    s12 = slope x1 x2 y1 y2
-- Initialization and fallback if all points are removed during pooling.
pool [x] [y] [] x' y' = (x' : [x], y' : [y], [slope x x' y y'])
pool xs ys ss x y =
  error $
    "pool: xs, ys, ss, x, y: "
      ++ show xs
      ++ ", "
      ++ show ys
      ++ ", "
      ++ show ss
      ++ ", "
      ++ show x
      ++ ", "
      ++ show y
      ++ "."
{-# SPECIALIZE pool ::
  [Int] ->
  [Double] ->
  [Double] ->
  Int ->
  Double ->
  ([Int], [Double], [Double])
  #-}
{-# SPECIALIZE pool ::
  [Double] ->
  [Double] ->
  [Double] ->
  Double ->
  Double ->
  ([Double], [Double], [Double])
  #-}

-- | Greatest convex minorant. Uses the Pool Adjacent Violators Algorithm
-- (PAVA). It is required that the predictors are ordered with no ties, and that
-- the lengths of the vectors are equal.
--
-- Usage:
--
-- @
--  gcm predictors responses = (indices, values, slopes)
-- @
gcm ::
  (Real a, Real b, Show a, Vector v a, Show b, Vector v b, Vector v Bool) =>
  v a ->
  v b ->
  ([a], [b], [Double])
gcm ps rs
  | lPs /= lRs =
      error $
        "gcm: Number of predictors is "
          ++ show lPs
          ++ ", but number of responses is "
          ++ show lRs
          ++ "."
  | not (strictlyOrdered ps) =
      error "gcm: The predictors are not strictly ordered."
  | otherwise =
      unsafeGcm ps rs
  where
    lPs = V.length ps
    lRs = V.length rs

-- | See 'gcm'.
--
-- Assume that:
-- - the lengths of the provided vectors are equal;
-- - the predictors are ordered.
unsafeGcm ::
  (Real a, Real b, Show a, Vector v a, Show b, Vector v b) =>
  v a ->
  v b ->
  ([a], [b], [Double])
unsafeGcm ps rs
  | l == 0 = ([], [], [])
  | l == 1 = start
  | otherwise = reverse3 $ go start (1 :: Int)
  where
    l = V.length rs
    start = ([V.head ps], [V.head rs], [])
    -- xs and ys: x and y values of gcm
    -- i: next index of rs
    go (xs, ys, ss) i
      | i >= l = (xs, ys, ss)
      | otherwise = go (pool xs ys ss x' y') (i + 1)
      where
        x' = ps V.! i
        y' = rs V.! i
