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
module HCad.Assembly where

import Algebra.Linear
import Algebra.Classes
import Prelude (Functor(..),error,Monad(..))
import Control.Applicative
import Data.Foldable
import Data.Traversable
import GHC.TypeLits

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

constrainV :: Linear v => v Expr -> v Expr -> Assembly ()
constrainV t1 t2 = sequence_ (constrain <$> t1 <*> t2)

constrainT :: (Applicative t,Traversable t) => Linear v => t (v Expr) -> t (v Expr) -> Assembly ()
constrainT t1 t2 = sequence_ (constrainV <$> t1 <*> t2)


variable :: Assembly (Expr)
variable = error "todo"

freeV :: Linear t => Assembly (t (Expr))
freeV = sequence (pure variable)

class Pointed t where
  point :: x -> t x

freeS :: (Pointed t,Traversable t) => Linear v => Assembly (t (v (Expr)))
freeS = sequence (point freeV)


freeRotation :: forall v. Pointed v => Lin v => Assembly (Mat v Expr)
freeRotation = do
  r <- freeS :: Assembly (Mat v Expr)
  constrainT r identity
  return r



data NamedVec (faces::[Symbol]) vec where
  NilVec :: NamedVec '[] vec
  ConsVec :: vec -> NamedVec xs vec -> NamedVec (x ': xs) vec

deriving instance (Functor (NamedVec faces))
deriving instance (Foldable (NamedVec faces))
deriving instance (Traversable (NamedVec faces))

class (∈) (x :: Symbol) (xs :: [Symbol]) where
  getVertex :: NamedVec xs a -> a

instance x ∈ (x ': xs) where
  getVertex (ConsVec x _) = x

instance x ∈ xs => x ∈ (y ': xs) where
  getVertex (ConsVec _y xs) = getVertex @x xs


