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
import Data.List


data Options = Options {optFn :: Int}

defaultOptions :: Options
defaultOptions = Options {optFn = 10}

render :: Options -> Part xs v Double -> String
render Options{..} p = unlines (("$fn="++show optFn++";"):
                                renderCode (partCode p)++
                                [";"])


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

