{- |
Module      :  Main
Copyright   :  (c) Dominik Schrempf, 2021
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Sun Jun  7 10:49:26 2020.

-}

module Main
  ( main
  )
where

import           Control.Monad
import           Criterion.Main
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector )
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           Statistics.Gcm

genPredictors :: Int -> Vector Int
genPredictors n = V.iterateN n succ 0

genResponses :: Int -> GenIO -> IO (Vector Double)
genResponses n g = V.fromList <$> replicateM n (exponential 1.0 g)

main :: IO ()
main = do
  g <- create
  let p1e3 = genPredictors 1000
      p1e4 = genPredictors 10000
      p1e5 = genPredictors 100000
  d1e3 <- genResponses 1000 g
  d1e4 <- genResponses 10000 g
  d1e5 <- genResponses 100000 g
  defaultMain
    [ bgroup
        "Greatest convex minorant"
        [ bench "Vector of length 1e3" $ nf (gcm p1e3) d1e3
        , bench "Vector of length 1e4" $ nf (gcm p1e4) d1e4
        , bench "Vector of length 1e5" $ nf (gcm p1e5) d1e5
        ]
    ]
