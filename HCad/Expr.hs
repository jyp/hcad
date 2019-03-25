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
module HCad.Expr where


import Algebra.Classes as A
import Prelude hiding (divMod,div,Num(..))
import Prelude (abs)
import qualified Prelude
import qualified Data.Set as Set
import Numeric (showGFloat)
data Expr where
  Con :: Double -> Expr
  Var :: Parameter -> Expr
  Fun :: String -> [Expr] -> Expr
  BinOp :: String -> Expr -> Expr -> Expr
  deriving Eq

pattern (:+:) :: Expr -> Expr -> Expr
pattern x :+: y = BinOp "+" x y

-- pattern (:-:) :: Expr -> Expr -> Expr
-- pattern x :-: y = BinOp "-" x y

pattern (:*:) :: Expr -> Expr -> Expr
pattern x :*: y = BinOp "*" x y

instance Additive Expr where
  zero = Con 0
  Con z + x | abs z < 1e-10 = x
  x + Con z | abs z < 1e-10 = x
  Con x + Con y = Con (x A.+ y)
  (Con x :+: z) + Con y = Con (x A.+ y) A.+ z
  Con y + (Con x :+: z) = Con (x A.+ y) A.+ z
  (Con a :+: x) + (Con b :+: y) = Con (a + b) :+: (x + y)
  (Con x :+: z) + y = Con x :+: (z + y)
  y + (Con x :+: z) = Con x :+: (z + y)
  (a :*: x) + (b :*: y) | x == y = (a + b) * x
  (a :*: x) + y | x == y = (a + 1) * x
  y + (a :*: x)| x == y = (a + 1) * x
  x + Con y = Con y :+: x
  x + y = x :+: y

instance Group Expr where
  -- x - Con z | abs z < 1e-10= x
  -- Con x - Con y = Con (x A.- y)
  -- x - y = BinOp "-" x y
  negate = (Con (-1) *)

instance Division Expr where
  Con x / Con y = Con (x A./ y)
  x / y = BinOp "/" x y


instance Multiplicative Expr where
  Con z * _  | abs z < 1e-10 = zero
  _ * Con z  | abs z < 1e-10 = zero
  Con 1 * x = x
  x * Con 1 = x
  Con x * Con y = Con (x A.* y)
  (x :+: y) * z = x * z + y * z
  z * (x :+: y) = x * z + y * z
  -- z * (x :-: y) = x * z - y * z
  (Con x :*: z) * Con y = Con (x A.* y) A.* z
  Con y * (Con x :*: z) = Con (x A.* y) A.* z
  x * Con y = Con y :*: x
  x * y = BinOp "*" x y
  one = Con 1

instance Show Expr where
  showsPrec d = \case
    BinOp op x y -> showParen True (showsPrec d x . showString op . showsPrec d y)
    Con x -> showGFloat (Just 8) x
    Var v -> showString (paramName v)
    Fun f args -> showString f . showParen True (intercalate' "," $ map (showsPrec d) args)

intercalate' :: Foldable t => String -> t (String -> String) -> String -> String
intercalate' s = foldr1 (\x y -> x . showString s . y)

data Possible = PRange Double Double | PSet [Double]
    deriving (Ord,Eq)
data Parameter
  = Parameter {paramGroup :: String
              ,paramName :: String
              ,paramDefault :: Double
              ,paramComment :: String
              ,paramPossible :: Possible}
    deriving (Ord,Eq)
params :: Expr -> Set.Set Parameter
params = \case
  BinOp _ x y -> foldMap params [x,y]
  Con _ -> Set.empty
  Var p -> Set.singleton p
  Fun _ args -> foldMap params args

confun :: (Double -> Double) -> String -> Expr -> Expr
confun f _g (Con x) = Con (f x)
confun _f g x = Fun g [x]

instance Module Expr Expr where
  (*^) = (A.*)
instance AbelianAdditive Expr where
instance Ring Expr where
instance Field Expr where
instance Floating Expr where
  pi = Con pi
  exp = confun exp "exp"
  log = confun log "log"
  sin = confun sin "sin"
  cos = confun cos "cos"
  asin = confun asin "asin"
  acos = confun acos "acos"
  atan = confun atan "atan"
  sinh = confun sinh "sinh"
  cosh = confun cosh "cosh"
  asinh = confun asinh "asinh"
  acosh = confun acosh "acosh"
  atanh = confun atanh "atanh"

instance Fractional Expr where
  fromRational = A.fromRational
  recip = A.recip

instance Prelude.Num Expr where
  (+) = (A.+)
  (-) = (A.-)
  (*) = (A.*)
  abs = Fun "abs" . return
  signum = Fun "signum" . return
  fromInteger = Con . A.fromInteger
