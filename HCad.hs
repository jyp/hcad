{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Algebra.Classes
import Prelude (Float,cos,sin,Floating(..),Functor(..),Show(..),Eq(..),flip,error,Double,Monad(..))
import Control.Applicative
import Data.Foldable
import Data.Traversable

data V2 a = V2 a a deriving (Functor,Foldable,Traversable,Show,Eq)
data V3 a = V3 a a a deriving (Functor,Foldable,Traversable,Show,Eq)

instance Lin V3 where
  identity = V3 (V3 one zero zero) (V3 zero one zero) (V3 zero zero one)

instance Applicative V3 where
  pure x = V3 x x x
  V3 f1 f2 f3 <*> V3 x1 x2 x3 = V3 (f1 x1) (f2 x2) (f3 x3)

instance Applicative V2 where
  pure x = V2 x x
  V2 f1 f2 <*> V2 x1 x2 = V2 (f1 x1) (f2 x2)

instance Additive a => Additive (V3 a) where
  zero = pure zero
  x + y =  (+) <$> x <*> y

instance Additive a => AbelianAdditive (V3 a) where

instance Ring a => Module a (V3 a) where
  s *^ t = (s*) <$> t

instance Ring a => Module a (V3 (V3 a)) where
  s *^ t = (s*^) <$> t

class (Applicative v, Traversable v) => Lin v where
  identity :: Ring a => Mat v a

type Linear v s = (Applicative v, Foldable v, Traversable v, Ring s)
type Mat v s = v (v s) -- inner structure are rows

data Part faces vectors where

translate :: Additive (v s) => Functor t => v s -> t (v s) -> t (v s)
translate v t = (v +) <$> t

scale :: VectorSpace s (v s) => Functor t => s -> t (v s) -> t (v s)
scale s t = (s *^) <$> t

(⊙) :: Linear v s => v s -> v s -> v s
x ⊙ y = (*) <$> x <*> y

dotProd :: Linear v s => v s -> v s -> s
dotProd x y = add (x ⊙ y)

matVecMul :: Linear v s => v (v s) -> v s -> v s
matVecMul m v = dotProd v <$> m

transform :: Linear v s => Functor t => Mat v s -> t (v s) -> t (v s)
transform m t = matVecMul m <$> t


rotation2d :: Floating a => a -> V2 (V2 a)
rotation2d θ = V2 (V2 ( cos θ) (sin θ))
                  (V2 (-sin θ) (cos θ))

-- >>> rotation2d (pi/2)
-- V2 (V2 6.123233995736766e-17 1.0) (V2 (-1.0) 6.123233995736766e-17)

crossProductMatrix :: Group a => V3 a -> Mat V3 a
crossProductMatrix (V3 a1 a2 a3) = V3  (V3 zero (negate a3) a2)
                                       (V3 a3 zero (negate a1))
                                       (V3 (negate a2) a1 zero)

-- | Cross product https://en.wikipedia.org/wiki/Cross_product
(×) :: Ring a => V3 a -> V3 a -> V3 a
(V3 a1 a2 a3) × (V3 b1 b2 b3) = V3 (a2*b3 - a3*b2 )  (negate (a1*b3 - a3*b1 )) (a1*b2 - a2*b1) 

-- | Tensor product
(⊗) :: Linear v a => v a -> v a -> Mat v a
v1 ⊗ v2 = (⊙) <$> pure v1 <*> (pure <$> v2)

-- | 3d rotation around given axis
rotation3d :: Ring a => Floating a => a -> V3 a -> Mat V3 a
rotation3d θ u = cos θ *^ identity +
                 sin θ *^ crossProductMatrix u +
                 (1 - cos θ) *^ (u ⊗ u)

transpose :: Linear v a => Mat v a -> Mat v a
transpose = sequenceA

matMul :: Linear v a => Mat v a -> Mat v a -> Mat v a
matMul x y = transform x (transpose y)

-- >>> let t1 = rotation2d (1::Double) in matMul t1 (transpose t1)
-- V2 (V2 1.0 0.0) (V2 0.0 1.0)


data Assembly a
instance Functor Assembly
instance Applicative Assembly
instance Monad Assembly
data Expr
instance AbelianAdditive Expr
instance Additive Expr
instance Multiplicative Expr
instance Group Expr
instance Ring Expr

constrain :: Expr -> Expr -> Assembly ()
constrain = error "todo"

constrainV :: Linear v Expr => v Expr -> v Expr -> Assembly ()
constrainV t1 t2 = sequence_ (constrain <$> t1 <*> t2)

constrainT :: (Applicative t,Traversable t) => Linear v Expr => t (v Expr) -> t (v Expr) -> Assembly ()
constrainT t1 t2 = sequence_ (constrainV <$> t1 <*> t2)


variable :: Assembly (Expr)
variable = error "todo"

freeV :: Linear t (Expr) => Assembly (t (Expr))
freeV = sequence (pure variable)

class Pointed t where
  point :: x -> t x

freeS :: (Pointed t,Traversable t) => Linear v (Expr) => Assembly (t (v (Expr)))
freeS = sequence (point freeV)


freeRotation :: forall v. Pointed v => Lin v => Assembly (v (v (Expr)))
freeRotation = do
  r <- freeS :: Assembly (Mat v Expr)
  constrainT r identity
  return r
