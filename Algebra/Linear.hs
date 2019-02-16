{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Algebra.Linear where

import Algebra.Classes
import Prelude (cos,sin,Floating(..),Functor(..),Show(..),Eq(..),Int,snd,fst,flip,($),Double)
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Control.Monad.State
import Control.Category

data V2' a = V2' a a deriving (Functor,Foldable,Traversable,Show,Eq)
data V3' a = V3' a a a deriving (Functor,Foldable,Traversable,Show,Eq)

-- | Make a Euclidean vector out of a traversable functor
newtype Euclid f a = Euclid (f a) deriving (Functor,Foldable,Traversable,Show,Eq,Applicative)

type Ring' a = Module a a

type V3 = Euclid V3'
type V2 = Euclid V2'
pattern V2 :: forall a. a -> a -> Euclid V2' a
pattern V2 x y = Euclid (V2' x y)
pattern V3 :: forall a. a -> a -> a -> Euclid V3' a
pattern V3 x y z = Euclid (V3' x y z)

instance Applicative V3' where
  pure x = V3' x x x
  V3' f1 f2 f3 <*> V3' x1 x2 x3 = V3' (f1 x1) (f2 x2) (f3 x3)

instance Applicative V2' where
  pure x = V2' x x
  V2' f1 f2 <*> V2' x1 x2 = V2' (f1 x1) (f2 x2)

instance (Applicative f,Additive a) => Additive (Euclid f a) where
  zero = pure zero
  x + y =  (+) <$> x <*> y
instance (Applicative f,AbelianAdditive a) => AbelianAdditive (Euclid f a) where
instance (Applicative f,Group a) => Group (Euclid f a) where
  negate x = negate <$> x
  x - y = (-) <$> x <*> y

instance (Applicative f,Module s a) => Module s (Euclid f a) where
  s *^ t = (s*^) <$> t

pureMat :: (Applicative v, Applicative w) => s -> Mat s v w
pureMat x = Mat (pure (pure x))

(>*<) :: (Applicative v, Applicative w) =>
               Mat (a -> s) v w -> Mat a v w -> Mat s v w
Mat f >*< Mat x = Mat (((<*>) <$> f) <*> x)

(>$<) :: (Applicative v, Applicative w) =>
               (a -> s) -> Mat a v w -> Mat s v w
f >$< x = pureMat f >*< x

instance (Applicative f,Applicative g,Additive a) => Additive (Mat a f g) where
  zero = pureMat zero
  x + y =  (+) >$< x >*< y
instance (Applicative f,Applicative g,AbelianAdditive a) => AbelianAdditive (Mat a f g) where
instance (Applicative f,Applicative g,Group a) => Group (Mat a f g) where
  negate x = negate >$< x
  x - y = (-) >$< x >*< y

instance (Applicative f, Applicative g,Module s a) => Module s (Mat a f g) where
  s *^ Mat t = Mat (((s*^) <$>) <$> t)

class VectorSpace (Scalar v) v => InnerProdSpace v where
  type Scalar v
  dotProd :: v -> v -> Scalar v

(⊙) :: Applicative v => Multiplicative s => v s -> v s -> v s
x ⊙ y = (*) <$> x <*> y

instance (Ring' a, Field a, Applicative f, Foldable f) => InnerProdSpace (Euclid f a) where
  type Scalar (Euclid f a) = a
  dotProd x y = add (x ⊙ y)

(·) :: InnerProdSpace v => v -> v -> Scalar v
(·) = dotProd

sqNorm :: InnerProdSpace v => v -> Scalar v
sqNorm x = dotProd x x

norm :: InnerProdSpace v => Floating (Scalar v) => v  -> Scalar v
norm = sqrt . sqNorm

normalize :: Floating (Scalar v) => InnerProdSpace v => v -> v
normalize v = recip (norm v) *^ v

-- | Cross product https://en.wikipedia.org/wiki/Cross_product
(×) :: Ring a => V3 a -> V3 a -> V3 a
(V3 a1 a2 a3) × (V3 b1 b2 b3) = V3 (a2*b3 - a3*b2)  (negate (a1*b3 - a3*b1)) (a1*b2 - a2*b1)

index :: Applicative v => Traversable v => v Int
index = fst (runState (sequenceA (pure increment)) zero)
  where increment = do x <- get; put (x+1); return x

type SqMat v s = Mat s v v
newtype Mat s v w = Mat {fromMat :: v (w s)} deriving Show
type Mat3x3 s = SqMat V3' s
type Mat2x2 s = SqMat V2' s

pattern Mat2x2 :: forall s. s -> s -> s -> s -> Mat s V2' V2'
pattern Mat2x2 a b c d = Mat (V2' (V2' a b) (V2' c d))

pattern Mat3x3 :: forall s. s -> s -> s -> s -> s -> s -> s -> s -> s -> Mat s V3' V3'
pattern Mat3x3 a b c d e f g h i = Mat (V3' (V3' a b c) (V3' d e f) (V3' g h i))

matVecMul :: (Foldable f1, Ring b, Applicative f1, Functor f2) => Mat b f2 f1 -> Euclid f1 b -> Euclid f2 b
matVecMul (Mat m) v = Euclid (euclideanDotProd v <$> (Euclid <$> m))
   where euclideanDotProd x y = add (Euclid x ⊙ Euclid y)


rotation2d :: Floating a => a -> Mat2x2 a
rotation2d θ = Mat $ V2' (V2' (cos θ) (-sin θ))
                         (V2' (sin θ)  (cos θ))


-- >>> rotation2d (pi/2)
-- Mat {fromMat = V2' (V2' 6.123233995736766e-17 (-1.0)) (V2' 1.0 6.123233995736766e-17)}

crossProductMatrix :: Group a => V3 a -> Mat3x3 a
crossProductMatrix (V3 a1 a2 a3) = Mat (V3'  (V3' zero (negate a3) a2)
                                             (V3' a3 zero (negate a1))
                                             (V3' (negate a2) a1 zero))


-- | Tensor product
(⊗) :: (Applicative v, Applicative w, Multiplicative s)
    => Euclid w s -> Euclid v s -> Mat s v w
(⊗) = flip (tensorWith (*))

tensorWith :: (Applicative v, Applicative w)
           => (s -> t -> u) -> Euclid w s -> Euclid v t -> Mat u w v
tensorWith f (Euclid v1) (Euclid v2) = flip f >$< Mat (pure v2) >*< Mat (pure <$> v1)

identity :: Traversable v => Ring s => Applicative v => SqMat v s
identity = tensorWith (\x y -> if x == y then one else zero) index index

-- | 3d rotation around given axis
rotation3d :: Ring' a => Floating a => a -> V3 a -> Mat3x3 a
rotation3d θ u = cos θ *^ identity +
                 sin θ *^ crossProductMatrix u +
                 (1 - cos θ) *^ (u ⊗ u)

rotationFromTo :: (Floating a, Module a a,Field a)
               => V3 a -> V3 a -> Mat3x3 a
rotationFromTo from to = c *^ identity + s *^ crossProductMatrix v + (1-c) *^ (v ⊗ v)
  where y = to
        x = from
        v = x × y
        c = dotProd x y
        s = norm v

transpose :: Applicative g => Traversable f => Mat a f g -> Mat a g f
transpose = Mat . sequenceA . fromMat

matMul :: (Traversable u, Ring s, Applicative w, Applicative v, Applicative u) => Mat s u v -> Mat s w u -> Mat s w v
matMul (transpose -> Mat y) (Mat x)  = tensorWith (\a b -> add (a ⊙ b)) (Euclid x) (Euclid y)

-- >>> let t1 = rotation2d (1::Double) in matMul t1 (transpose t1)
-- Mat {fromMat = V2' (V2' 1.0 0.0) (V2' 0.0 1.0)}

