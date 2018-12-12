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

import Algebra.Linear hiding (scale,translate,transform)
import qualified Algebra.Linear as Li
import Algebra.Classes
import Prelude hiding (Num(..),(/))
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

getLoc :: forall x xs a. x ∈ xs => RelLoc xs a
getLoc p = Loc (getVertex @x p) (getNormal @x p)

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



forget :: Part xs vec -> Part '[] vec
forget Part{..} = Part {partNormals=Nil,partVertices=Nil,..}

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
              ,partCode = SCAD "sphere" [("d","1"),(("$fn","20"))] []}

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
              ,partCode = SCAD "circle" [("d","1"),("$fn","20")] []}

polygon :: Show a => [V2 a] -> Part '[] (V2 a)
polygon points
  = Part {partVertices = Nil
         ,partNormals = Nil
         ,partCode = SCAD "polygon" [("points",showL (map renderVec points))] []}


regularPolygon :: Division a => Floating a => Show a => Int -> Part '[] (V2 a)
regularPolygon order = polygon (coords)
  where coords=[V2 (cos th) (sin th)
               | i <- [0..order-1],
                 let th = fromIntegral i*(2.0*pi/fromIntegral order) ];


linearExtrude :: forall a xs. Module a a => Fractional a => Show a
              => a -> Part xs (V2 a) -> Part (SimpleFields '["bottom","top"] ++ xs) (V3 a)
linearExtrude height Part{..}
  = Part {partVertices = (((0.5 * height) *^) <$> botTopNormals) ++* (z0 <$> partVertices)
         ,partNormals = botTopNormals ++* (z0 <$> partNormals)
         ,partCode = SCAD "linear_extrude"
                     [("height",show height)
                     ,("center","true")
                     ,("convexity","10")
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


(/+) :: Part xs v -> Part ys v -> Part (xs ++ ys) v
(/+) p1 p2 = Part {partVertices = partVertices p1 ++* partVertices p2
                  ,partNormals = partNormals p1 ++* partNormals p2
                  ,partCode = SCAD "union" [] [partCode p1,partCode p2]}
union :: Part ys v -> Part xs v -> Part (xs ++ ys) v
union = flip (/+)

unions :: [Part xs v] -> Part '[] v
unions ps = Part {partVertices = Nil
                 ,partNormals = Nil
                 ,partCode = SCAD "union" [] (map partCode ps)}

intersection :: Part ys v -> Part xs v -> Part (xs ++ ys) v
intersection p2 p1 = Part {partVertices = partVertices p1 ++* partVertices p2
                          ,partNormals = partNormals p1 ++* partNormals p2
                          ,partCode = SCAD "intersection" [] [partCode p1,partCode p2]}

(/-) :: Part xs v -> Part ys v -> Part (xs ++ ys) v
(/-) p1 p2 = Part {partVertices = partVertices p1 ++* partVertices p2
                   ,partNormals = partNormals p1 ++* partNormals p2
                   ,partCode = SCAD "difference" [] [partCode p1,partCode p2]}



difference :: Part ys v -> Part xs v -> Part (xs ++ ys) v
difference = flip (/-)

translate :: forall (v :: Type -> Type) s xs. Foldable v => Show s => Additive (v s) => v s -> Part xs (v s) -> Part xs (v s)
translate v Part{..} = Part {partNormals = partNormals
                            ,partVertices = (v +) <$> partVertices
                            ,partCode = SCAD "translate" [("v",renderVec v)] [partCode]
                            }
transform :: Show s => Floating s => Division s => Module s s => Ring s => Mat V3 s -> Part xs (V3 s) -> Part xs (V3 s)
transform m Part{..} = Part {partVertices = Li.transform m partVertices
                            ,partNormals = Li.normalize . matVecMul m <$> partVertices
                            ,partCode = SCAD "multmatrix" [("m",m')] [partCode]}
  where m' = showL (toList (showL .  toList . (fmap show) <$> m) ++ ["[0,0,0,1]"])
          
scale' :: Traversable v => Applicative v => (Module s (v s), Field s,Show s) => v s -> Part xs (v s) -> Part xs (v s)
scale' v Part{..} = Part {partNormals = partNormals
                        ,partVertices = (v ⊙) <$> partVertices
                        ,partCode = SCAD "scale" [("v",renderVec v)] [partCode] }

------------------------------------------------

data Loc v = Loc {locPoint :: v, locNormal :: v}
type RelLoc xs v = Part xs v -> Loc v

at :: (Foldable v, Show s, Group (v s)) => (RelLoc xs (v s)) -> (Part xs (v s) -> Part ys (v s)) -> (Part xs (v s) -> Part ys (v s))
at relLoc f body = (translate loc . f . translate (negate loc)) body where
  loc = locPoint (relLoc body)

data PartAndLoc ys xs vec = PartAndLoc (Part ys vec -> Part xs vec)

(/@) :: Part xs (v s) -> (PartAndLoc xs zs (v s)) -> Part zs (v s)
p /@ (PartAndLoc f) = f p
infixl 8 /@

-- (@+) :: (Foldable v, Show s, Group (v s)) => (Part ys (v s) -> (v s)) -> Part xs (v s) -> PartAndLoc ys (ys ++ xs) (v s)
-- f @+ p = PartAndLoc (\q -> q /+ at (f q) p)
-- infixl 9 @+

-- (@-) :: (Foldable v, Show s, Group (v s)) => (Part ys (v s) -> (v s)) -> Part xs (v s) -> PartAndLoc ys (ys ++ xs) (v s)
-- f @- p = PartAndLoc (\q -> q /- at (f q) p)
-- infixl 9 @-


scale :: (Applicative v,Module s (v s), Field s,Traversable v,Show s) => s -> Part xs (v s) -> Part xs (v s)
scale s = scale' (pure s)



------------------------------------------------
-- Non-primitive ops

pocket :: Show a => Fractional a => Module a a
       => a -> Part ys (V2 a) -> (Part xs (V3 a) -> Part (xs ++ '[]) (V3 a))
pocket depth shape body = body /- forget negative  where
  negative = translate (V3 0 0 (epsilon - 0.5 * depth)) (linearExtrude (depth+2*epsilon) shape)
  epsilon = 0.05

on :: Division a => Module a a => Floating a => Group a => Additive a => Show a
   => RelLoc xs (V3 a) -> (Part xs (V3 a) -> Part ys (V3 a)) -> (Part xs (V3 a) -> Part ys (V3 a))
on relLoc f body = translate locPoint $ transform (Li.transpose normUp) $ f $ transform normUp $ translate (negate locPoint) $ body
  where Loc{..} = relLoc body
        normUp = rotationFromTo locNormal (V3 o o 1)
        o = zero

-- at :: (Foldable v, Show s, Group (v s)) => (RelLoc xs (v s)) -> (Part xs (v s) -> Part ys (v s)) -> (Part xs (v s) -> Part ys (v s))
-- at relLoc f body = (translate loc . f . translate (negate loc)) body where
--   loc = locPoint (relLoc body)

-- orient3dTo :: forall x xs a. Module a a => Ord a => Floating a => Division a => Ring a => x ∈ xs => V3 a -> Part xs (V3 a) -> Part xs (V3 a)
-- orient3dTo x p@Part{..} = matVecMul r <$> p
--   where y = getField @x partNormals
--         v = x × y
--         c = dotProd x y
--         s = norm v
--         r = c *^ identity + s *^ crossProductMatrix v + (1-c) *^ (v ⊗ v)

centering :: Show a => Foldable v => Group (v a) => RelLoc xs (v a) -> Part xs (v a) -> Part xs (v a)
centering getX p = translate (negate (locPoint (getX p))) p



--------------------------------------
-- Locations

south :: '["front"] ∈ xs => RelLoc xs (v a); south = getLoc @'["front"]
north :: '["back"] ∈ xs => RelLoc xs (v a); north = getLoc @'["back"]
west  :: '["left"] ∈ xs => RelLoc xs (v a); west = getLoc @'["left"]
east  :: '["right"] ∈ xs => RelLoc xs (v a); east = getLoc @'["right"]

yxPoint :: V2 a -> V2 a -> V2 a
yxPoint (Lin (V2' _ y)) (Lin (V2' x _)) = V2 x y

yxLoc :: (t -> Loc (V2 a)) -> (t -> Loc (V2 a)) -> t -> Loc (V2 a)
yxLoc f g p = Loc (yxPoint (locPoint y) (locPoint x)) (yxPoint (locNormal y) (locNormal x))
  where y = f p
        x = g p


type East = "right"
type West = "left"
type North = "back"
type South = "front"

southWest :: ('[South] ∈ xs, '[West] ∈ xs) => RelLoc xs (V2 a); southWest = yxLoc south west
southEast :: ('[South] ∈ xs, '[East] ∈ xs) => RelLoc xs (V2 a); southEast = yxLoc south east
northEast :: ('[North] ∈ xs, '[East] ∈ xs) => RelLoc xs (V2 a); northEast = yxLoc north east
northWest :: ('[North] ∈ xs, '[West] ∈ xs) => RelLoc xs (V2 a); northWest = yxLoc north west



-------------------------------------
-- Rendering

renderVec :: (Show a, Foldable t) => t a -> String
renderVec v = showL (map show (toList v))

showL :: [String] -> String
showL v = "[" <> intercalate ", " v <> "]"



renderCode :: SCAD -> [String]
renderCode (SCAD fname args body)
  | fname == "union" = rbody
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



