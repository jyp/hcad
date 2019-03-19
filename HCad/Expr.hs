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
import Prelude hiding (divMod,div)
import Data.List (intercalate)

data Expr where
  Con :: String -> Expr
  Var :: String -> String -> Double -> Expr
  Fun :: String -> [Expr] -> Expr
  BinOp :: String -> Expr -> Expr -> Expr

instance Additive Expr where
  zero = Con "0"
  (+) = BinOp "+"

instance Group Expr where
  (-) = BinOp "-"

instance Division Expr where
  (/) = BinOp "/"

instance Multiplicative Expr where
  (*) = BinOp "*"
  one = Con "1"

parens :: [Char] -> [Char]
parens x = "(" ++ x ++ ")"
instance Show Expr where
  show = \case
    BinOp op x y -> parens (show x ++ op ++ show y)
    Con x -> x
    Var v _ _ -> v
    Fun f args -> f ++ parens (intercalate "," $ map show args)

params :: Expr -> [(String,String,Double)]
params = \case
  BinOp _ x y -> params x ++ params y
  Con _ -> []
  Var a b c -> [(a,b,c)]
  Fun _ args -> concatMap params args

confun :: String -> Expr -> Expr
confun x = Fun x . return

instance AbelianAdditive Expr where
instance Ring Expr where
instance Field Expr where
instance Floating Expr where
  pi = Con "pi"
  exp = confun "exp"
  log = confun "log"
  sin = confun "sin"
  cos = confun "cos"
  asin = confun "asin"
  acos = confun "acos"
  atan = confun "atan"
  sinh = confun "sinh"
  cosh = confun "cosh"
  asinh = confun "asinh"
  acosh = confun "acosh"
  atanh = confun "atanh"

instance Fractional Expr where
  fromRational = A.fromRational
  recip = A.recip
instance Num Expr where
  (+) = (A.+)
  (-) = (A.-)
  (*) = (A.*)
  abs = Fun "abs" . return
  signum = Fun "signum" . return
  fromInteger = A.fromInteger
