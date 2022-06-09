{- |
   Module      :  Statistics.GcmSpec
   Description :  Unit tests for Statistics.Gcm
   Copyright   :  2021 Dominik Schrempf
   License     :  GPL-3.0-or-later

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Fri Jun  5 17:01:22 2020.

-}

module Statistics.GcmSpec
  ( spec
  )
where

import           Test.Hspec
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector )

import           Statistics.Gcm

ps :: Vector Int
ps = V.fromList [0 .. 9]

rs :: Vector Double
rs = V.fromList [19, 16, 17, 12, 19, 6, 13, 1, 4, 0]

xs :: [Int]
xs = [0, 1, 7, 9]

ys :: [Double]
ys = [19.0, 16.0, 1.0, 0.0]

ss :: [Double]
ss = [-3.0, -2.5, -0.5]

ps2 :: Vector Int
ps2 = V.fromList [0 .. 23]

rs2 :: Vector Double
rs2 = V.fromList
  [ 2.39604875
  , 0.59133253
  , 0.49601200
  , 0.77828088
  , 0.36678584
  , 0.81875424
  , 0.59677332
  , 2.32285757
  , 0.13094589
  , 0.45258170
  , 0.10324180
  , 2.56827353
  , 0.50313726
  , 0.31220387
  , 1.33664320
  , 1.24568427
  , 0.80642147
  , 1.69867356
  , 0.03597119
  , 0.82510016
  , 0.95890956
  , 2.90166793
  , 0.67492229
  , 0.21564363
  ]

xs2 :: [Int]
xs2 = [0, 1, 2, 4, 8, 10, 18, 23]

ys2 :: [Double]
ys2 =
  [ 2.39604875
  , 0.59133253
  , 0.496012
  , 0.36678584
  , 0.13094589
  , 0.1032418
  , 3.597119e-2
  , 0.21564363
  ]

ss2 :: [Double]
ss2 =
  [ -1.80471622
  , -9.532052999999996e-2
  , -6.461308000000002e-2
  , -5.895998749999999e-2
  , -1.3852045000000007e-2
  , -8.40882625e-3
  , 3.5934488e-2
  ]

spec :: Spec
spec = describe "gcm" $ do
  it "returns the greatest convex minorant" $ do
    gcm ps rs `shouldBe` (xs, ys, ss)
    gcm ps2 rs2 `shouldBe` (xs2, ys2, ss2)
  it "shouldn't fail on a singleton vector"
    $ gcm (V.singleton 0 :: Vector Int) (V.singleton 1.0 :: Vector Double)
    `shouldBe` ([0], [1.0], [])
  it "shouldn't fail on an empty vector"
    $          gcm (V.empty :: Vector Int) (V.empty :: Vector Double)
    `shouldBe` ([], [], [])
