{- |
Module      :  Main
Copyright   :  (c) Dominik Schrempf, 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Jun  7 10:49:26 2020.

-}

module Main
  ( main
  ) where

import Control.Monad
import Criterion.Main
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import System.Random.MWC
import System.Random.MWC.Distributions

import Statistics.Gcm

genData :: Int -> GenIO -> IO (Vector Double)
genData n g = V.fromList <$> replicateM n (exponential 1.0 g)

main :: IO ()
main = do
  g <- create
  d1e3 <- genData 1000 g
  d1e5 <- genData 10000 g
  d1e7 <- genData 1000000 g
  defaultMain
    [ bgroup "Greatest convex minorant"
      [ bench "Vector of length 1e3" $ nf gcm d1e3
      , bench "Vector of length 1e5" $ nf gcm d1e5
      , bench "Vector of length 1e7" $ nf gcm d1e7 ]
    ]
