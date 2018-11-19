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
module HCad.Assembly where

import Algebra.Linear
import Algebra.Classes
import Prelude (Functor(..),error,Monad(..),Double,Int)
import Control.Applicative
import Data.Foldable
import Data.Traversable
import GHC.TypeLits
import Control.Monad.State

import Data.LinearProgram.LinExpr

type Var = Int
type Expr = LinExpr Var Double

newtype Assembly a = Assembly {fromAsm :: State Int a} deriving (Functor,Monad,Applicative)

-- instance AbelianAdditive Expr
-- instance Additive Expr
-- instance Multiplicative Expr
-- instance Group Expr
-- instance Ring Expr

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


-- freeRotation :: forall v. Pointed v => Lin v => Assembly (Mat v Expr)
-- freeRotation = do
--   r <- freeS :: Assembly (Mat v Expr)
--   constrainT (matMul r (transpose r)) (diagonal (LinExpr zero one)) -- No (Ring Expr)!
--   return r


