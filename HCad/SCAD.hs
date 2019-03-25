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
module HCad.SCAD where

import HCad.Part
import HCad.Expr
import Data.List (intercalate, nub)
-- import Algebra.Linear
import Data.Set (Set)
import qualified Data.Set as Set
data Options = Options {optFn :: Int}

defaultOptions :: Options
defaultOptions = Options {optFn = 10}

render :: Functor v => Foldable v => Options -> Part xs v Double -> String
render Options{..} p = unlines (("$fn="++show optFn++";"):
                                renderCode (toSCAD $ partCode p)++
                                [";"])

renderP :: Functor v => Foldable v => Options -> Part xs v Expr -> String
renderP Options{..} p = unlines (("$fn="++show optFn++";"):
                                 concat [["// " ++ paramComment
                                         ,paramName ++ " = " ++ show paramDefault ++ "; "
                                          ++ "// " ++ v (paramPossible)]
                                        | Parameter{..} <- Set.toList ps ] ++
                                 renderCode (toSCAD $ partCode p) ++
                                 [";"])
  where ps :: Set Parameter
        ps = foldMap params (partCode p)
        v (PRange lo hi) = "["++show lo++":"++ show hi ++"]"
        v (PSet vals) = show vals


renderCode :: SCAD -> [String]
renderCode (SCAD fname args body)
  -- | fname == "union" = rbody
  | otherwise = (fname <>"(" <> (intercalate ", " [pname <> "=" <> arg
                                                                      | (pname,arg) <- args]) <> ")") `app` rbody
  where rbody = case body of
          [] -> []
          [x] -> renderCode x
          xs -> "{" : fmap indent (concatMap (semicolon . renderCode) xs) ++ "}" : []

        indent xs = " " ++ xs
        semicolon [] = error "semicolon: empty"
        semicolon xs = init xs ++ [last xs ++ ";"]
        x `app` (y : ys) = (x<>y) : ys
        app x [] = [x]



-- tst :: Part3 '[] Double
-- tst = forget $ mirror (V3 (sin (pi/6)) (cos (pi/6)) 0) $ translate (V3 20 0 0) $ on zenith (union $ translate (V3 0 2.5 0) $ scale 5 $ cube) $ scale 10 cube

-- main :: IO ()
-- main = writeFile "tst.scad" $ render defaultOptions tst

-- >>> main
