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
module HCad.Part where

import Algebra.Linear as Li
import Algebra.Classes
import Prelude (Functor(..),Floating(..),(.),Ord(..))
import Control.Applicative
import Data.Foldable
import Data.Traversable
import GHC.TypeLits

data Part xs vec
  = Part {partVertices :: NamedVec xs vec
         ,partNormals  :: NamedVec xs vec}
    deriving Functor


data NamedVec (fields::[Symbol]) vec where
  Nil :: NamedVec '[] vec
  (:*) :: vec -> NamedVec xs vec -> NamedVec (x ': xs) vec
infixr :*

deriving instance (Functor (NamedVec faces))
deriving instance (Foldable (NamedVec faces))
deriving instance (Traversable (NamedVec faces))

class (∈) (x :: Symbol) (xs :: [Symbol]) where
  getField :: NamedVec xs a -> a

instance x ∈ (x ': xs) where
  getField (x :* _) = x

instance x ∈ xs => x ∈ (y ': xs) where
  getField (_y :* xs) = getField @x xs

getNormal :: forall x xs a. x ∈ xs => Part xs a -> a
getNormal = getField @x . partNormals

getVertex :: forall x xs a. x ∈ xs => Part xs a -> a
getVertex = getField @x . partVertices

orient3dTo :: forall x xs a. Ord a => Floating a => Division a => Ring a => x ∈ xs => V3 a -> Part xs (V3 a) -> Part xs (V3 a)
orient3dTo x p@Part{..} = matVecMul r <$> p
  where y = getField @x partNormals
        v = x × y
        c = dotProd x y
        s = norm v
        r = c *^ identity + s *^ crossProductMatrix v + (1-c) *^ (v ⊗ v)

orient3d :: forall x y xs ys a. Ord a => Floating a => Division a => Ring a => x ∈ xs => y ∈ ys =>
            Part xs (V3 a) -> Part ys (V3 a) -> Part ys (V3 a)
orient3d p1 = orient3dTo @y (getNormal @x p1)

buttTo :: forall x xs a v. Group (v a) => x ∈ xs => v a -> Part xs (v a) -> Part xs (v a)
buttTo x p = HCad.Part.translate (x - y) p
  where y = getVertex @x p

butt :: forall x y xs ys a v. Group (v a) => x ∈ xs => y ∈ ys => Part xs (v a) -> Part ys (v a) -> Part ys (v a)
butt p1 = buttTo @y (getVertex @x p1)

translate :: Additive (v s) => v s -> Part xs (v s) -> Part xs (v s)
translate v Part{..} = Part {partNormals = partNormals, partVertices = Li.translate v partVertices}

scale :: (Module s (v s), Field s) => s -> Part xs (v s) -> Part xs (v s)
scale v Part{..} = Part {partNormals = partNormals, partVertices = Li.scale v partVertices}

box :: Ring a => V3 a -> Part '["left", "right", "front", "back", "bottom", "top"] (V3 a)
box v = Part {partVertices = (v ⊙) <$> partNormals ,..}
  where partNormals =
           V3 j o o :*
           V3 i o o :*
           V3 o j o :*
           V3 o i o :*
           V3 o o j :*
           V3 o o i :*
           Nil
        i = one
        o = zero
        j = negate i

