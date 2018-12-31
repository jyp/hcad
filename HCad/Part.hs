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

import Algebra.Linear hiding (transform)
import qualified Algebra.Linear as Li
import Algebra.Classes
import Prelude hiding (Num(..),(/),divMod,div)
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

cube :: forall a. Module a a => Fractional a => Show a => Ring a
     => Part (SimpleFields '[West, East, South, North, Nadir, Zenith]) (V3 a)
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
              ,partCode = SCAD "sphere" [("r","0.5"),(("$fn","20"))] []}

square :: forall a. Module a a => Fractional a => Show a => Ring a
       => Part (SimpleFields '["center", West, East, South, North]) (V2 a)
square = Part {partVertices = zero :* (((0.5 :: a) *^) <$> corners)
              ,partNormals = V2 o j :* corners
              ,partCode = SCAD "square" [("size","1"),("center","true")] []}
  where corners =
           V2 j o :*
           V2 i o :*
           V2 o j :*
           V2 o i :*
           Nil
        i = one
        o = zero
        j = negate i

rectangle sz = scale' sz  square

circle :: Part '[] (V2 a)
circle = Part {partVertices = Nil, partNormals = Nil
              ,partCode = SCAD "circle" [("r","0.5"),("$fn","20")] []}

polygon :: Show a => [V2 a] -> Part '[] (V2 a)
polygon points
  = Part {partVertices = Nil
         ,partNormals = Nil
         ,partCode = SCAD "polygon" [("points",showL (map renderVec points))] []}



linearExtrude :: forall a xs. Division a => Floating a => Module a a => Fractional a => Show a
              => a -> Part xs (V2 a) -> Part (SimpleFields '[Nadir,Zenith] ++ xs) (V3 a)
linearExtrude height p = linearExtrudeEx height 1 0 p


linearExtrudeEx :: forall a xs. Floating a => Division a => Module a a => Fractional a => Show a
              => a -> a -> a -> Part xs (V2 a) -> Part (SimpleFields '[Nadir,Zenith] ++ xs) (V3 a)
linearExtrudeEx height scaleFactor twist Part{..}
  = Part {partVertices = (((0.5 * height) *^) <$> botTopNormals) ++* (z0 <$> partVertices)
         ,partNormals = botTopNormals ++* (z0 <$> partNormals)
         ,partCode = SCAD "linear_extrude"
                     [("height",show height)
                     ,("center","true")
                     ,("convexity","10")
                     ,("scale",show scaleFactor)
                     ,("twist",showAngle twist )
                     ]
                     [partCode]
         }
    where botTopNormals = V3 o o (negate one) :*
                          V3 o o one :*
                          Nil
          o = zero
          z0 (Lin (V2' x y)) = (V3 x y o)


flattenUnions :: [SCAD] -> [SCAD]
flattenUnions (SCAD "union" [] xs:ys) = xs ++ flattenUnions ys
flattenUnions (x:xs) = x:flattenUnions xs
flattenUnions [] = []

mkUnion :: [SCAD] -> SCAD
mkUnion xs = SCAD "union" [] (flattenUnions xs)

(/+) :: Part xs v -> Part ys v -> Part (xs ++ ys) v
(/+) p1 p2 = Part {partVertices = partVertices p1 ++* partVertices p2
                  ,partNormals = partNormals p1 ++* partNormals p2
                  ,partCode = mkUnion [partCode p1,partCode p2]}
union :: Part ys v -> Part xs v -> Part (xs ++ ys) v
union = flip (/+)

unions :: [Part xs v] -> Part '[] v
unions ps = Part {partVertices = Nil
                 ,partNormals = Nil
                 ,partCode = mkUnion (map partCode ps)}

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
                            ,partNormals = Li.normalize <$> Li.transform m partNormals
                            ,partCode = SCAD "multmatrix" [("m",m')] [partCode]}
  where m' = showL (toList (showL . ( ++ ["0"]) . toList . (fmap show) <$> m) ++ ["[0,0,0,1]"])

scale' :: Traversable v => Applicative v => (Module s (v s), Field s,Show s) => v s -> Part xs (v s) -> Part xs (v s)
scale' v Part{..} = Part {partNormals = partNormals
                        ,partVertices = (v ⊙) <$> partVertices
                        ,partCode = SCAD "scale" [("v",renderVec v)] [partCode] }


scale :: (Applicative v,Module s (v s), Field s,Traversable v,Show s) => s -> Part xs (v s) -> Part xs (v s)
scale s = scale' (pure s)

------------------------------------------------
-- Locations and relative locations

data Loc v = Loc {locPoint :: v, locNormal :: v}

-- | Origin point with normal pointing to 'Zenith'.
origin :: Ring a => Loc (V3 a)
origin = Loc {locPoint = zero, locNormal = V3 zero zero one}

type RelLoc xs v = Part xs v -> Loc v

-- | Put the focus point on the given point (not changing the focused
-- direction)
at :: (Foldable v, Show s, Group (v s)) => (RelLoc xs (v s)) -> (Part xs (v s) -> Part ys (v s)) -> (Part xs (v s) -> Part ys (v s))
at relLoc f body = translating (locPoint (relLoc body)) f body

translating :: (Foldable v, Show s, Group (v s)) =>
                     v s
                     -> (Part xs1 (v s) -> Part xs2 (v s))
                     -> Part xs1 (v s)
                     -> Part xs2 (v s)
translating delta f = translate delta . f . translate (negate delta)

-- | Put the focus point over or under the given point (so, leaving
-- z-coordinate unchanged)
atXY :: (Show s, Division s, Module s s) =>
              (Part xs (Lin V3' s) -> Loc (Lin V3' s))
              -> (Part xs (Lin V3' s) -> Part ys (Lin V3' s))
              -> Part xs (Lin V3' s)
              -> Part ys (Lin V3' s)
atXY f = at (projectOnPlane origin . f)


transforming :: (Show s, Floating s, Division s, Module s s) =>
                      Mat (Lin V3') s
                      -> (Part xs1 (V3 s) -> Part xs2 (V3 s))
                      -> Part xs1 (V3 s)
                      -> Part xs2 (V3 s)
transforming o f = transform (Li.transpose o) . f . transform o

-- | Put the focus point on the given location (point and direction)
on :: Division a => Module a a => Floating a => Group a => Additive a => Show a
   => RelLoc xs (V3 a) -> (Part xs (V3 a) -> Part ys (V3 a)) -> (Part xs (V3 a) -> Part ys (V3 a))
on relLoc f body = translating locPoint (transforming normUp f) body
  where Loc{..} = relLoc body
        normUp = rotationFromTo locNormal (V3 o o 1)
        o = zero

-- | Center the given location
center :: Show a => Foldable v => Group (v a) => RelLoc xs (v a) -> Part xs (v a) -> Part xs (v a)
center getX p = translate (negate (locPoint (getX p))) p



------------------------------------------------
-- Non-primitive ops

-- | Regular polygon contained a unit-diameter circle.
regularPolygon :: Field a => Module a a => Division a => Floating a => Show a => Int -> Part '[] (V2 a)
regularPolygon order = scale 0.5 (polygon (coords))
  where coords=[V2 (cos th) (sin th)
               | i <- [0..order-1],
                 let th = fromIntegral i*(2.0*pi/fromIntegral order) ];

-- | Regular polygon containing a unit-diameter circle.
regularPolygonO :: Field a => Module a a => Division a => Floating a => Show a => Int -> Part '[] (V2 a)
regularPolygonO order = scale (1 / cos (pi / fromIntegral order)) $ regularPolygon order

-- | Create a mortise
push :: (a ~ Double)
       => a -> Part ys (V2 a) -> (Part xs (V3 a) -> Part (xs ++ '[]) (V3 a))
push depth shape = difference $ forget $ 
                   translate (V3 0 0 (epsilon - 0.5 * depth)) (linearExtrude (depth+2*epsilon) shape)
  where epsilon = 0.05

-- | Create a tenon
pull :: (a ~ Double)
       => a -> Part ys (V2 a) -> (Part xs (V3 a) -> Part (xs ++ '[]) (V3 a))
pull depth shape = union $ forget $ translate (V3 0 0 (0.5 * depth)) (linearExtrude depth shape)

counterSink :: (Floating a, Show a, Module a a, Field a) =>
                     a
                     -> a
                     -> Part xs (Lin V3' a)
                     -> Part (xs ++ '[]) (Lin V3' a)
counterSink angle diameter = difference (forget negative)  where
  negative = (translate (V3 0 0 (epsilon - 0.5 * diameter * c)) $ transform (rotation3d pi (V3 1 0 0)) (scale diameter (linearExtrudeEx c 0 0 circle )))
  epsilon = 0.05
  c = sin angle

----------------------------------
-- Filling

linearRepeat' :: (Foldable v, Show s, Module Int (v s)) =>
                Int -> [v s] -> Part xs (v s) -> Part '[] (v s)
linearRepeat' number intervals part =
  unions [translate (k *^ (intervals !! k) + j *^ add intervals) part
         | i <- [negate number `div` 2..number `div` 2],
           let (j,k) = i `divMod` length intervals
         ]

linearRepeat :: (Foldable v, Show s, Module Int (v s)) =>
                Int -> v s -> Part xs (v s) -> Part '[] (v s)
linearRepeat number interval part =
  unions [translate (i *^ interval) part
         | i <- [negate number `div` 2..number `div` 2]]

linearFill :: (Show s, Module Int (v s), RealFrac s, Division s,
                     Traversable v, Applicative v, Ring s, Floating s) =>
                    s -> v s -> Part xs (v s) -> Part '[] (v s)
linearFill len interval part = linearRepeat (floor (len / norm interval)) interval part

-- | Fill a rectangle in hexagonal pattern
hexagonFill :: Module Int s => RealFrac s => Floating s => Show s => Field s => Module s s
               => s -> s -> s
               -> Part xs (V2 s)
               -> Part (' ['["center"], '[West], '["right"], '[South], '[North]]) (V2 s)
hexagonFill len width cell_size shape
  = intersection (scale' (V2 len width) square) $
    linearRepeat' no_of_rows (V2 tr_x <$> [negate tr_y, tr_y]) $
    linearFill width (V2 0 cell_size) $ 
    shape
  where no_of_rows = floor(1.2 * len / cell_size)
        tr_x = sqrt(3)/2 * cell_size
        tr_y = cell_size / 2


--------------------------------------
-- Locations

south :: '[South] ∈ xs => RelLoc xs (v a); south = getLoc @'[South]
north :: '[North] ∈ xs => RelLoc xs (v a); north = getLoc @'[North]
west  :: '[West] ∈ xs => RelLoc xs (v a); west = getLoc @'[West]
east  :: '[East] ∈ xs => RelLoc xs (v a); east = getLoc @'[East]
nadir :: '[Nadir] ∈ xs => RelLoc xs (v a); nadir = getLoc @'[Nadir]
zenith :: '[Zenith] ∈ xs => RelLoc xs (v a); zenith = getLoc @'[Zenith]


projectOnPlane :: (Applicative v, Traversable v, Module scalar (v scalar), Group (v scalar))
          => Loc (v scalar) -> Loc (v scalar) -> Loc (v scalar)
projectOnPlane Loc {locNormal = planeNormal
              , locPoint = planeOrigin}
          Loc {..} = Loc {locPoint = position, locNormal = locNormal}
 where θ = (planeOrigin - locPoint) · planeNormal
       position = θ *^ planeNormal + locPoint
       -- equation : (position - planeOrigin) · planeNormal = 0

(|<-) :: (Applicative v, Traversable v,
                Module scalar (v scalar), Group (v scalar)) =>
               (t -> Loc (v scalar))
               -> (t -> Loc (v scalar)) -> t -> Loc (v scalar)
(plane |<- pos) p = projectOnPlane (plane p) (pos p)
infixr |<-

projectOnLine :: (Applicative v, Traversable v,
                        Module scalar (v scalar), Group (v scalar)) =>
                       Loc (v scalar) -> Loc (v scalar) -> Loc (v scalar)
projectOnLine Loc {locNormal = lineNormal, locPoint = lineOrigin}
              Loc {..} = Loc {locPoint = position, locNormal = locNormal}
  where cosθ = (locPoint - lineOrigin) · lineNormal 
        position = lineOrigin + cosθ *^ lineNormal 

(/<-) :: (Applicative v, Traversable v, Module scalar (v scalar),
                Group (v scalar)) =>
               (t -> Loc (v scalar))
               -> (t -> Loc (v scalar)) -> t -> Loc (v scalar)
(line /<- pos) p = projectOnLine (line p) (pos p)


projectOnPoint :: (Applicative v, Traversable v,
                        Module scalar (v scalar), Group (v scalar)) =>
                       Loc (v scalar) -> Loc (v scalar) -> Loc (v scalar)
projectOnPoint Loc {locPoint = lineOrigin}
              Loc {..} = projectOnLine Loc {locNormal=locNormal, locPoint=lineOrigin} Loc {..}

(.<-) :: (Applicative v, Traversable v, Module scalar (v scalar),
                Group (v scalar)) =>
               (t -> Loc (v scalar))
               -> (t -> Loc (v scalar)) -> t -> Loc (v scalar)
(line .<- pos) p = projectOnPoint (line p) (pos p)

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
type Zenith = "top"
type Nadir = "bottom"

southEast :: LinearSpace v a => Division a => ('[South] ∈ xs, '[East] ∈ xs) => RelLoc xs (v a); southEast p = projectOnPlane (south p) (east p)
southWest :: LinearSpace v a => Division a => ('[South] ∈ xs, '[West] ∈ xs) => RelLoc xs (v a); southWest p = projectOnPlane (south p) (west p)
northEast :: LinearSpace v a => Division a => ('[North] ∈ xs, '[East] ∈ xs) => RelLoc xs (v a); northEast p = projectOnPlane (north p) (east p)
northWest :: LinearSpace v a => Division a => ('[North] ∈ xs, '[West] ∈ xs) => RelLoc xs (v a); northWest p = projectOnPlane (north p) (west p)


-------------------------------------
-- Rendering

renderVec :: (Show a, Foldable t) => t a -> String
renderVec v = showL (map show (toList v))

showL :: [String] -> String
showL v = "[" <> intercalate ", " v <> "]"

showAngle :: Show a => Division a => Floating a => a -> String
showAngle x = show (x * (180 / pi))



