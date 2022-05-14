{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
module HCad.Nuts where

import Algebra.Linear
import Algebra.Classes
import Prelude hiding (Num(..),(/),divMod,div,recip,fromRational)
import HCad.Part

data MNut a = MNut { mSize :: a
                   , mThreadPitch :: a
                   , mMaxFlatsDist :: a
                   , mMinFlatsDist :: a
                   , mMaxThickness :: a
                   , mMinThickness :: a}
m1_6,m2, m2_5, m3, m4, m5, m6, m8, m10, m12, m14, m16, m20, m24, m30, m36, m42, m48, m56, m64 :: Field a => MNut a 

m1_6  = MNut 1.6    0.35    3.2     3.02     1.3     1.05
m2    = MNut 2      0.4     4       3.82     1.6     1.35
m2_5  = MNut 2.5    0.45    5       4.82     2       1.75
m3    = MNut 3      0.5     5.5     5.32     2.4     2.15
m4    = MNut 4      0.7     7       6.78     3.2     2.9
m5    = MNut 5      0.8     8       7.78     4.7     4.4
m6    = MNut 6      1       10      9.78     5.2     4.9
m8    = MNut 8      1.25    13      12.73    6.8     6.44
m10   = MNut 10     1.5     16      15.73    8.4     8.04
m12   = MNut 12     1.75    18      17.73    10.8    10.37
m14   = MNut 14     2       21      20.67    12.8    12.1
m16   = MNut 16     2       24      23.67    14.8    14.1
m20   = MNut 20     2.5     30      29.16    18      16.9
m24   = MNut 24     3       36      35       21.5    20.2
m30   = MNut 30     3.5     46      45       25.6    24.3
m36   = MNut 36     4       55      53.8     31      29.4
m42   = MNut 42     4.5     65      63.1     34      32.4
m48   = MNut 48     5       75      73.1     38      36.4
m56   = MNut 56     5.5     85      82.8     45      43.4
m64   = MNut 64     6       95      92.8     51      49.1



metricNutProfile :: Transcendental a => Module a a => Show a => Field a => MNut a -> a -> Part '[] V2 a
metricNutProfile nut tol = scale flat2flat $ regularPolygonO 6
  where flat2flat = mMaxFlatsDist nut + tol

metricBoltProfile :: Transcendental a => Module a a => Show a => Field a => MNut a -> a -> Part '[] V2 a
metricBoltProfile m tol = scale (mSize m + tol) $ circle

metricNutSocket :: (Transcendental a, Show a, Module a a, Field a) => MNut a -> a -> a -> a -> Part3 xs a -> Part3 xs a
metricNutSocket m tol recess depth = push recess (metricNutProfile m tol) . push depth (metricBoltProfile m tol)

