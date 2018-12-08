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
module HCad.Part where

import Algebra.Linear hiding (scale,translate)
import qualified Algebra.Linear as Li
import Algebra.Classes
import Prelude hiding (Num(..))
import Data.Foldable
import GHC.TypeLits
import Data.List hiding (union)
import Data.Kind (Type)

data SCAD = SCAD {scadPrim :: String
                 ,scadArgs :: [(String,String)]
                 ,scadBody :: [SCAD]}

data Part xs vec
  = Part {partVertices :: NamedVec xs vec
         ,partNormals  :: NamedVec xs vec
         ,partCode :: SCAD }
    deriving Functor

type family (++) (a::[k]) (b::[k]) where
  '[] ++ a = a
  (x ': xs) ++ ys = x ': (xs ++ ys)

infixr ++*
(++*) :: NamedVec xs v -> NamedVec ys v -> NamedVec (xs ++ ys) v
Nil ++* ys = ys
(x :* xs) ++* ys = x :* xs ++* ys

type FieldName = [Symbol]

data NamedVec (fields::[FieldName]) vec where
  Nil :: NamedVec '[] vec
  (:*) :: vec -> NamedVec xs vec -> NamedVec (x ': xs) vec

infixr :*

-- class KnownLen xs where
--   repet :: a -> NamedVec xs a

-- instance KnownLen xs => Applicative (NamedVec xs) where
--   pure = repet
--   Nil <*> Nil = Nil
--   (f :* fs) <*> (a :* as) = f a :* (fs <*> as)
  
-- instance (Additive vec, KnownLen xs) => Additive (NamedVec xs vec) where
--   zero = repet zero
--   v1 + v2 = (+) <$> v1 <*> v2
-- instance (AbelianAdditive vec, KnownLen xs) => AbelianAdditive (NamedVec xs vec)
-- instance Module s vec => Module s (NamedVec xs vec)
  
deriving instance (Functor (NamedVec faces))
deriving instance (Foldable (NamedVec faces))
deriving instance (Traversable (NamedVec faces))

class (∈) (x :: FieldName) (xs :: [FieldName]) where
  getField :: NamedVec xs a -> a

instance {-# OVERLAPPING #-} x ∈ (x ': xs) where
  getField (x :* _) = x

instance {-# OVERLAPPING #-} x ∈ xs => x ∈ (y ': xs) where
  getField (_y :* xs) = getField @x xs


class (⊆) (xs :: [FieldName]) (ys :: [FieldName]) where
  filterVec :: NamedVec ys a -> NamedVec xs a

instance {-# OVERLAPPING #-} xs ⊆ ys => xs ⊆ (x ': ys) where
  filterVec (_ :* xs) = filterVec xs

instance {-# OVERLAPPING #-} xs ⊆ ys => (x ': xs) ⊆ (x ': ys) where
  filterVec (x :* xs) = x :* filterVec xs

instance {-# OVERLAPPING #-} '[] ⊆ '[] where
  filterVec Nil = Nil

getNormal :: forall x xs a. x ∈ xs => Part xs a -> a
getNormal = getField @x . partNormals

getVertex :: forall x xs a. x ∈ xs => Part xs a -> a
getVertex = getField @x . partVertices

class KnownD v where
  is3d :: Bool

-------------------------------------------
-- Primitive ops

type family SimpleFields x where
  SimpleFields '[]  = '[]
  SimpleFields ( x ': xs)  = '[x] ': SimpleFields xs

type family MapCons x xs where
  MapCons _ '[] = '[]
  MapCons x ( y ': ys) = ( (x ': y) ': MapCons x ys )

nameVec :: forall x xs vec. NamedVec xs vec -> NamedVec (MapCons x xs) vec
nameVec Nil = Nil
nameVec (a :* as) = (a :* nameVec @x as)


name :: forall x xs vec. Part xs vec -> Part (MapCons x xs) vec
name (Part {..}) = Part{partVertices = nameVec @x partVertices
                           ,partNormals = nameVec @x partNormals
                           ,..}

weaken :: ys ⊆ xs => Part xs vec -> Part ys vec
weaken (Part {..}) = Part{partVertices = filterVec partVertices
                         ,partNormals = filterVec partNormals
                         ,..}


forget :: ('[] ⊆ xs) => Part xs vec -> Part '[] vec
forget = weaken

cube :: forall a. Module a a => Fractional a => Show a => Ring a => Part (SimpleFields '["left", "right", "front", "back", "bottom", "top"]) (V3 a)
cube = Part {partVertices = ((0.5 :: a) *^) <$> partNormals,..}
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
        partCode = SCAD "cube" [("size","1"),("center","true")] []

sphere :: Part '[] (V3 a)
sphere = Part {partVertices = Nil, partNormals = Nil
              ,partCode = SCAD "sphere" [("d","1")] []}

square :: forall a. Module a a => Fractional a => Show a => Ring a => Part (SimpleFields '["left", "right", "front", "back"]) (V2 a)
square = Part {partVertices = ((0.5 :: a) *^) <$> partNormals,..}
  where partNormals =
           V2 j o :*
           V2 i o :*
           V2 o j :*
           V2 o i :*
           Nil
        i = one
        o = zero
        j = negate i
        partCode = SCAD "square" [("size","1"),("center","true")] []

rectangle sz = scale' sz  square

circle :: Part '[] (V2 a)
circle = Part {partVertices = Nil, partNormals = Nil
              ,partCode = SCAD "circle" [("d","1")] []}

-- TODO: polygon

linearExtrude :: forall a xs. Module a a => Fractional a => Show a
              => a -> Part xs (V2 a) -> Part (SimpleFields '["bottom","top"] ++ xs) (V3 a)
linearExtrude height Part{..}
  = Part {partVertices = (((0.5 :: a) *^) <$> botTopNormals) ++* (z0 <$> partVertices)
         ,partNormals = botTopNormals ++* (z0 <$> partNormals)
         ,partCode = SCAD "linear_extrude"
                     [("height",show height)
                     -- ,("twist",0)
                     -- ,("scale",)
                     ]
                     [partCode]
         }
    where botTopNormals = V3 o o (negate one) :*
                          V3 o o one :*
                          Nil
          o = zero
          z0 (Lin (V2' x y)) = (V3 x y o)


union,(/+) :: Part xs v -> Part ys v -> Part (xs ++ ys) v
union p1 p2 = Part {partVertices = partVertices p1 ++* partVertices p2
                   ,partNormals = partNormals p1 ++* partNormals p2
                   ,partCode = SCAD "union" [] [partCode p1,partCode p2]}
(/+) = union

difference, (/-) :: Part xs v -> Part ys v -> Part (xs ++ ys) v
difference p1 p2 = Part {partVertices = partVertices p1 ++* partVertices p2
                   ,partNormals = partNormals p1 ++* partNormals p2
                   ,partCode = SCAD "difference" [] [partCode p1,partCode p2]}


(/-) = difference
------------------------------------------------

at :: (Foldable v, Show s, Group (v s)) => v s -> Part xs (v s) -> Part xs (v s)
at v = translate v

data PartAndLoc ys xs vec = PartAndLoc (Part ys vec -> Part xs vec)

(/@) :: Part xs (v s) -> (PartAndLoc xs zs (v s)) -> Part zs (v s)
p /@ (PartAndLoc f) = f p
infixl 8 /@

(@+) :: (Foldable v, Show s, Group (v s)) => (Part ys (v s) -> (v s)) -> Part xs (v s) -> PartAndLoc ys (ys ++ xs) (v s)
f @+ p = PartAndLoc (\q -> q /+ at (f q) p)
infixl 9 @+

(@-) :: (Foldable v, Show s, Group (v s)) => (Part ys (v s) -> (v s)) -> Part xs (v s) -> PartAndLoc ys (ys ++ xs) (v s)
f @- p = PartAndLoc (\q -> q /- at (f q) p)
infixl 9 @-

------------------------------------------------
-- Non-primitive ops


orient3dTo :: forall x xs a. Module a a => Ord a => Floating a => Division a => Ring a => x ∈ xs => V3 a -> Part xs (V3 a) -> Part xs (V3 a)
orient3dTo x p@Part{..} = matVecMul r <$> p
  where y = getField @x partNormals
        v = x × y
        c = dotProd x y
        s = norm v
        r = c *^ identity + s *^ crossProductMatrix v + (1-c) *^ (v ⊗ v)

orient3d :: forall x y xs ys a. Module a a => Ord a => Floating a => Division a => Ring a => x ∈ xs => y ∈ ys =>
            Part xs (V3 a) -> Part ys (V3 a) -> Part ys (V3 a)
orient3d p1 = orient3dTo @y (getNormal @x p1)

centering :: Show a => Foldable v => Group (v a) => (Part xs (v a) -> v a) -> Part xs (v a) -> Part xs (v a)
centering getX p = translate (negate (getX p)) p

translate :: forall (v :: Type -> Type) s xs. Foldable v => Show s => Additive (v s) => v s -> Part xs (v s) -> Part xs (v s)
translate v Part{..} = Part {partNormals = partNormals
                            ,partVertices = (v +) <$> partVertices
                            ,partCode = SCAD "translate" [("v",renderVec v)] [partCode]
                            }

scale :: (Applicative v,Module s (v s), Field s,Traversable v,Show s) => s -> Part xs (v s) -> Part xs (v s)
scale s = scale' (pure s)

scale' :: Traversable v => Applicative v => (Module s (v s), Field s,Show s) => v s -> Part xs (v s) -> Part xs (v s)
scale' v Part{..} = Part {partNormals = partNormals
                        ,partVertices = (v ⊙) <$> partVertices
                        ,partCode = SCAD "scale" [("v",renderVec v)] [partCode] }

-------------------------------------
-- Rendering 

renderVec :: (Show a, Foldable t) => t a -> String
renderVec v = "[" <> intercalate ", " (map show (toList v)) <> "]"

renderCode :: SCAD -> [String]
renderCode (SCAD "union" [] body) = concatMap renderCode body
renderCode (SCAD fname args body) = [fname <>"(" <> (intercalate ", " [pname <> "=" <> arg
                                                                      | (pname,arg) <- args]) <> ")" <> rbody]
  where rbody = case concatMap renderCode body of
          [] -> ""
          [x] -> x
          xs -> "{" <> mconcat ((<>";") <$> xs) <> "}"

south :: '["front"] ∈ xs => Part xs (V2 a) -> V2 a
south = getVertex @'["front"]

north :: '["back"] ∈ xs => Part xs (V2 a) -> V2 a
north = getVertex @'["back"]

west :: '["left"] ∈ xs => Part xs (V2 a) -> V2 a
west = getVertex @'["left"]

east :: '["right"] ∈ xs => Part xs (V2 a) -> V2 a
east = getVertex @'["right"]

yxPoint :: (t -> Lin V2' a) -> (t -> Lin V2' a) -> t -> Lin V2' a
yxPoint f g p = V2 x y
  where V2 _ y = f p
        V2 x _ = g p

type East = "right"
type West = "left"
type North = "back"
type South = "front"

southWest :: ('["front"] ∈ xs, '["left"] ∈ xs) => Part xs (V2 a) -> V2 a
southWest = yxPoint south west

southEast :: ('[South] ∈ xs, '[East] ∈ xs) => Part xs (V2 a) -> V2 a
southEast = yxPoint south east

northEast :: ('[North] ∈ xs, '[East] ∈ xs) => Part xs (V2 a) -> V2 a
northEast = yxPoint north east

northWest :: ('[North] ∈ xs, '[West] ∈ xs) => Part xs (V2 a) -> V2 a
northWest = yxPoint north west



