{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HCad.Part.Extensions where

import HCad.Part
import Algebra.Linear
import Algebra.Classes hiding (normalize)
import Algebra.Category
import Prelude hiding (Num(..),(/),divMod,div,recip,fromRational, (.), mod, id)

-- | Extrude a shape along a givent segment. The y axis of the shape
-- will align with the upwards direction given. This function may
-- crash if the segment is itself too well aligned with the segment.
extrudeAlongSegment :: (Show a,Transcendental a)
  => Part xs V2 a -- ^ shape
  -> V3 a -- ^ upwards direction
  -> (V3 a, V3 a) -- ^ segment
  -> Part (SimpleFields '[Nadir,Zenith] ++ xs) V3 a
extrudeAlongSegment shape upDir (start,end) = translate start $ rotate r $ center nadir $ extrude l shape
  where r = Mat (V3 x' (x' × z') z')
        l = norm d
        d = end-start
        z' = normalize d
        x' = normalize (upDir × z')

-- | Apply 'extrudeAlongSegment' on several segments
extrudeAlongSegments :: (Show a, Transcendental a)
  => Part xs V2 a -> V3 a -> [(V3 a, V3 a)] -> Part '[] V3 a
extrudeAlongSegments shape upDir = unions . map (extrudeAlongSegment shape upDir)
