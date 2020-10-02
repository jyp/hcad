{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
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
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Example where

import HCad
import Algebra.Linear ()
import Algebra.Classes hiding ((*<))
import Control.Category
import Prelude hiding (Integral, Num(..), (/), divMod, div, mod, fromRational, recip, id, (.))
import qualified Prelude

type R = Double

rndr :: Foldable v => Functor v => Part xs v R -> String
rndr = render myOpts

myOpts :: Options
myOpts = defaultOptions {optFn = 20}

model :: Part3 _ R
model =
  on zenith (counterSink 1 1 ) $
  on east (push 1 $ scale 3 circle) $
  on nadir (push 1 $ scale 3 circle) $
  on north (push 1 $ scale 3 circle) $
  on south (push 1 $ scale 3 circle) $
  on west (push 1 $ scale 3 circle) $
  (scale 5.0 cube)

main :: IO ()
main = do
  writeFile "base.scad" (rndr $ model)



-- >>> main

-- >>> locPoint (nadir (scale 2 cube))
-- <interactive>:737:2-32: warning: [-Wtype-defaults]
--     • Defaulting the following constraints to type ‘Double’
--         (Show a0) arising from a use of ‘print’ at <interactive>:737:2-32
--         (Field a0) arising from a use of ‘it’ at <interactive>:737:2-32
--         (Floating a0) arising from a use of ‘it’ at <interactive>:737:2-32
--     • In a stmt of an interactive GHCi command: print it
-- Euclid {fromEuclid = VNext (VNext (VNext VZero 0.0) (-1.2246467991473532e-16)) (-1.0)}


-- >>> zAxis
-- <interactive>:569:2-6: warning: [-Wtype-defaults]
--     • Defaulting the following constraints to type ‘Integer’
--         (Show a0) arising from a use of ‘print’ at <interactive>:569:2-6
--         (Ring a0) arising from a use of ‘it’ at <interactive>:569:2-6
--     • In a stmt of an interactive GHCi command: print it
-- Euclid {fromEuclid = VNext (VNext (VNext VZero 0) 0) 1}

-- >>> locBase (east model) `matVecMul` zAxis
-- Euclid {fromEuclid = VNext (VNext (VNext VZero (-1.0)) 0.0) 0.0}

-- >>> locBase (east model) `matVecMul` xAxis
-- Euclid {fromEuclid = VNext (VNext (VNext VZero 0.0) 2.2186712959340957e-31) 1.0}

-- >>> locBase (east model) `matVecMul` yAxis
-- Euclid {fromEuclid = VNext (VNext (VNext VZero 0.0) 1.0) 2.2186712959340957e-31}

-- >>> scale 1.2 sphere
-- <interactive>:661:2-17: error:
--     • No instance for (Show (Part '[] V3 Double))
--         arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it
