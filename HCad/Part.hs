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
import Prelude hiding (Num(..),(/),divMod,div,recip)
import Data.Foldable
import GHC.TypeLits
import Data.List (intercalate)
import Data.Kind (Type)
import Data.Type.Equality
import Unsafe.Coerce

data SCAD = SCAD {scadPrim :: String
                 ,scadArgs :: [(String,String)]
                 ,scadBody :: [SCAD]}


data Part xs vec a
  = Part {partVertices :: NamedVec xs (Euclid vec a) -- TODO: use Loc here
         ,partBases  :: NamedVec xs (SqMat vec a)
         ,partCode :: SCAD }

type Part3 xs a = Part xs V3' a
type Part2 xs a = Part xs V2' a

type family (++) (a::[k]) (b::[k]) where
  '[] ++ a = a
  (x ': xs) ++ ys = x ': (xs ++ ys)

unitR :: xs :~: (xs ++ '[])
unitR = unsafeCoerce Refl

(#>) :: a :~: b -> (a ~ b => k) -> k
Refl #> k = k
infixr 0 #>

infixr ++*
(++*) :: NamedVec xs v -> NamedVec ys v -> NamedVec (xs ++ ys) v
Nil ++* ys = ys
(x :* xs) ++* ys = x :* xs ++* ys

type FieldName = [Symbol]

data NamedVec (fields::[FieldName]) vec where
  Nil :: NamedVec '[] vec
  (:*) :: vec -> NamedVec xs vec -> NamedVec (x ': xs) vec

infixr :*


class KnownLen xs where
  repet :: a -> NamedVec xs a
  appl :: NamedVec xs (a -> b) -> NamedVec xs a -> NamedVec xs b

instance KnownLen '[] where
  repet _ = Nil
  appl _ _ = Nil


instance KnownLen xs => KnownLen (x ': xs) where
  repet x = x :* repet x
  (f :* fs) `appl` (a :* as) = f a :* (fs `appl` as)

instance KnownLen xs => Applicative (NamedVec xs) where
  pure = repet
  (<*>) = appl

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

getBase :: forall x xs v a. x ∈ xs => Part xs v a -> SqMat v a
getBase = getField @x . partBases

getVertex :: forall x xs v a. x ∈ xs => Part xs v a -> Euclid v a
getVertex = getField @x . partVertices

getLoc :: forall x xs v a. x ∈ xs => RelLoc xs v a
getLoc p = Loc (getVertex @x p) (getBase @x p)

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


name :: forall x xs vec a. Part xs vec a -> Part (MapCons x xs) vec a
name (Part {..}) = Part{partVertices = nameVec @x partVertices
                           ,partBases = nameVec @x partBases
                           ,..}

weaken :: ys ⊆ xs => Part xs vec a -> Part ys vec a
weaken (Part {..}) = Part{partVertices = filterVec partVertices
                         ,partBases = filterVec partBases
                         ,..}



forget :: Part xs vec a -> Part '[] vec a
forget Part{..} = Part {partBases=Nil,partVertices=Nil,..}

meshImport :: String -> Part3 '[] a
meshImport fname = Part {partBases=Nil
                  ,partVertices=Nil
                  ,partCode= SCAD "import" [("file",show fname)] []}

color' :: (Show s) => Double -> V3 s -> Part xs vec s -> Part xs vec s
color' a c Part{..} = Part {partCode = SCAD "color" [("c",renderVec c),("alpha",show a)] [partCode],..}

color :: (Show s) => V3 s -> Part xs vec s -> Part xs vec s
color = color' 1

cube :: Show a => Ring' a => Floating a => Division a
     => Part '[ '["bottom"], '["top"], '["right"], '["back"],
                        '["left"], '["front"], '["northEast"], '["northWest"],
                        '["southWest"], '["southEast"]] V3' a
cube = extrude one square

sphere :: Part3 '[] a
sphere = Part {partVertices = Nil, partBases = Nil
              ,partCode = SCAD "sphere" [("r","0.5"),(("$fn","20"))] []}

square :: forall a. Module a a => Floating a => Show a => Ring a
       => Part2 (SimpleFields '[East, North, West, South, "northEast", "northWest", "southWest", "southEast"]) a
square = Part {partVertices = matVecMul <$> partBases <*> (V2 <$> scales <*> pure 0)
              ,partCode = SCAD "square" [("size","1"),("center","true")] []
              ,..}
  where partBases = rotation2d <$> angles
        scales = 0.5 :* 0.5 :* 0.5 :* 0.5 :* sqrt 0.5 :* sqrt 0.5 :* sqrt 0.5 :* sqrt 0.5 :* Nil
        angles = (pi *) <$> (0   :* 0.5 :* 1   :* 1.5 :* 0.25     :* 0.75     :* 1.25     :* 1.75     :* Nil)

rectangle sz = scale' sz  square

circle :: Part2 '[] a
circle = Part {partVertices = Nil, partBases = Nil
              ,partCode = SCAD "circle" [("r","0.5"),("$fn","20")] []}

polygon :: Show a => [V2 a] -> Part2 '[] a
polygon points
  = Part {partVertices = Nil
         ,partBases = Nil
         ,partCode = SCAD "polygon" [("points",showL (map renderVec points))] []}



extrude :: forall a xs. Division a => Floating a => Module a a => Fractional a => Show a
              => a -> Part2 xs a -> Part3 (SimpleFields '[Nadir,Zenith] ++ xs) a
extrude height p = extrudeEx height 1 0 p


extrudeEx :: forall a xs. Floating a => Division a => Module a a => Fractional a => Show a
              => a -> a -> a -> Part2 xs a -> Part3 (SimpleFields '[Nadir,Zenith] ++ xs) a
extrudeEx height scaleFactor twist Part{..}
  = Part {partVertices = (flip matVecMul (V3 0 0 (0.5 * height)) <$> botTopBases) ++* (z0 <$> partVertices)
         ,partBases = botTopBases ++* (conv  <$> partBases)
         ,partCode = SCAD "linear_extrude"
                     [("height",show height)
                     ,("center","true")
                     ,("convexity","10")
                     ,("scale",show scaleFactor)
                     ,("twist",showAngle twist )
                     ]
                     [partCode]
         }
    where botTopBases = flip rotation3d (V3 1 0 0) <$> angles
          angles = pi :* zero :* Nil
          z0 (V2 x y) = (V3 x y zero)
          zz0 (Mat2x2 a b c d) =
             Mat3x3 a b 0
                    c d 0
                    0 0 1
          zToX = Mat3x3 0 0 1
                        0 1 0
                        (-1) 0 0
          conv m = transpose (zz0 m) `matMul` zToX `matMul` (zz0 m)

lathe :: (Show a, Division a, Floating a) => Part2 xs a -> Part3 '[] a
lathe = latheEx (2*pi)

latheEx :: (Show a, Division a, Floating a) => a -> Part2 xs a -> Part3 '[] a
latheEx angle Part{..} =
  Part {partVertices = Nil,
        partBases = Nil,
        partCode = SCAD "rotate_extrude" [("angle",showAngle angle),("$fn","50")] [partCode]
       }


flattenUnions :: [SCAD] -> [SCAD]
flattenUnions (SCAD "union" [] xs:ys) = xs ++ flattenUnions ys
flattenUnions (x:xs) = x:flattenUnions xs
flattenUnions [] = []

mkUnion :: [SCAD] -> SCAD
mkUnion xs = SCAD "union" [] (flattenUnions xs)

(/+) :: Part xs v a -> Part ys v a -> Part (xs ++ ys) v a
(/+) p1 p2 = Part {partVertices = partVertices p1 ++* partVertices p2
                  ,partBases = partBases p1 ++* partBases p2
                  ,partCode = mkUnion [partCode p1,partCode p2]}
union :: Part ys v a -> Part xs v a -> Part (xs ++ ys) v a
union = flip (/+)

unions :: [Part xs v a] -> Part '[] v a
unions ps = Part {partVertices = Nil
                 ,partBases = Nil
                 ,partCode = mkUnion (map partCode ps)}

intersection :: Part ys v a -> Part xs v a -> Part (xs ++ ys) v a
intersection p2 p1 = Part {partVertices = partVertices p1 ++* partVertices p2
                          ,partBases = partBases p1 ++* partBases p2
                          ,partCode = SCAD "intersection" [] [partCode p1,partCode p2]}

(/-) :: Part xs v a -> Part ys v a -> Part (xs ++ ys) v a
(/-) p1 p2 = Part {partVertices = partVertices p1 ++* partVertices p2
                   ,partBases = partBases p1 ++* partBases p2
                   ,partCode = SCAD "difference" [] [partCode p1,partCode p2]}



difference :: Part ys v a -> Part xs v a -> Part (xs ++ ys) v a
difference = flip (/-)

translate :: forall (v :: Type -> Type) s xs. Additive s => Applicative v => Foldable v => Show s =>  Euclid v s -> Part xs v s -> Part xs v s
translate v Part{..} = Part {partBases = partBases
                            ,partVertices = (v +) <$> partVertices
                            ,partCode = SCAD "translate" [("v",renderVec v)] [partCode]
                            }

rotate :: Traversable v => Applicative v => Show s => Floating s => Division s => Module s s => Ring s => SqMat v s -> Part xs v s -> Part xs v s
rotate m Part{..} = Part {partVertices = matVecMul m <$> partVertices
                         ,partBases = (\subBase -> m `matMul`subBase ) <$>  partBases
                         ,partCode = SCAD "multmatrix" [("m",m')] [partCode]}
  -- where m' = showL (toList (showL . ( ++ ["0"]) . toList . (fmap show) <$> fromMat m) ++ ["[0,0,0,1]"])
  where m' = showL (toList (showL . toList . (fmap show) <$> fromMat m))


mirror :: Foldable v => Show a => Euclid v a -> Part xs v a -> Part '[] v a
mirror normal p = Part {partBases=Nil
                       ,partVertices=Nil
                       ,partCode= SCAD "mirror" [("v",renderVec normal)] [partCode p]}

scale' :: Traversable v => Applicative v => (Field s,Show s) => Euclid v s -> Part xs v s -> Part xs v s
scale' v Part{..} = Part {partBases = partBases -- FIXME: shear the base!
                         ,partVertices = (v ⊙) <$> partVertices
                         ,partCode = SCAD "scale" [("v",renderVec v)] [partCode] }

scale :: (Applicative v,Field s,Traversable v,Show s) => s -> Part xs v s -> Part xs v s
scale s = scale' (pure s)

------------------------------------------------
-- Locations and relative locations

data Loc v a = Loc {locPoint :: Euclid v a, locBase :: SqMat v a}

locNormal :: Ring a => Loc V3' a -> Euclid V3' a
locNormal = flip matVecMul (V3 zero zero one) . locBase

-- | Origin point with normal pointing to 'Zenith'.
origin :: Ring a => Loc V3' a
origin = Loc {locPoint = zero, locBase = identity}

type RelLoc xs v a = Part xs v a -> Loc v a

-- | Put the focus point on the given point (not changing the focused
-- direction)
at :: (Foldable v, Applicative v, Show s, Group s) => (RelLoc xs v s) -> (Part xs v s -> Part ys v s) -> (Part xs v s -> Part ys v s)
at relLoc f body = translating (locPoint (relLoc body)) f body

translating :: (Applicative v,Foldable v, Show s, Group s) =>
                     Euclid v s
                     -> (Part xs1 v s -> Part xs2 v s)
                     -> Part xs1 v s
                     -> Part xs2 v s
translating delta f = translate delta . f . translate (negate delta)

-- -- | Put the focus point over or under the given point (so, leaving
-- -- z-coordinate unchanged)
-- atXY :: (Show s, Division s, Module s s) =>
--               (Part xs (V3 s) -> Loc (V3 s))
--               -> (Part xs (V3 s) -> Part ys (V3 s))
--               -> Part xs (V3 s)
--               -> Part ys (V3 s)
-- atXY f = at (projectOnPlane origin . f)


rotating :: Applicative v => Traversable v => (Show s, Floating s, Field s, Module s s) =>
                      SqMat v s
                      -> (Part xs1 v s -> Part xs2 v s)
                      -> Part xs1 v s
                      -> Part xs2 v s
rotating o f = rotate o . f . rotate (transpose o)

-- | Put the focus point on the given locus
on :: Traversable v => Applicative v => Division a => Module a a => Floating a => Field a => Show a
   => RelLoc xs v a -> (Part xs v a -> Part ys v a) -> (Part xs v a -> Part ys v a)
on relLoc f body = translating locPoint (rotating locBase f) body
  where Loc{..} = relLoc body

-- | Center the given location
center :: Applicative v => Show a => Foldable v => Group a => RelLoc xs v a -> Part xs v a -> Part xs v a
center getX p = translate (negate (locPoint (getX p))) p



------------------------------------------------
-- Non-primitive ops


xAxis :: V3 Double
xAxis = V3 1 0 0

yAxis :: V3 Double
yAxis = V3 0 1 0

zAxis :: V3 Double
zAxis = V3 0 0 1



-- | Regular polygon contained a unit-diameter circle.
regularPolygon :: Field a => Module a a => Division a => Floating a => Show a => Int -> Part2 '[] a
regularPolygon order = scale 0.5 (polygon coords)
  where coords=[V2 (cos th) (sin th)
               | i <- [0..order-1],
                 let th = fromIntegral i*(2.0*pi/fromIntegral order) ];

-- | Regular polygon containing a unit-diameter circle.
regularPolygonO :: Field a => Module a a => Division a => Floating a => Show a => Int -> Part2 '[] a
regularPolygonO order = scale (1 / cos (pi / fromIntegral order)) $ regularPolygon order

-- | Create a mortise
push :: forall xs ys.
        Double -> Part2 ys Double -> (Part3 xs Double -> Part3 xs Double)
push depth shape =
  unitR @xs #> (difference $ forget $ 
                translate (V3 0 0 (epsilon - 0.5 * depth)) (extrude (depth+2*epsilon) shape))
  where epsilon :: Double
        epsilon = 0.05

-- | Create a tenon
pull :: forall xs ys a. (a ~ Double)
       => a -> Part2 ys a -> (Part3 xs a -> Part3 xs a)
pull depth shape = unitR @xs #> union $ forget $ translate (V3 0 0 (0.5 * depth)) (extrude depth shape)

cone' :: (Floating a, Division a, Module a a, Show a) => a -> Part3 '[ '["bottom"], '["top"]] a
cone' angle = (extrudeEx c 0 0 circle)
  where c = sin angle

counterSink :: forall xs a.
  (Floating a, Show a, Module a a, Field a)
  => a -> a -> Part3 xs a -> Part3 xs a
counterSink angle diameter = unitR @xs #> difference (forget negative)  where
  negative = translate (V3 0 0 epsilon) $ center nadir $ rotate (rotation3d pi (V3 1 0 0)) (scale diameter $ cone' angle)
  epsilon = 0.05

----------------------------------
-- Filling

linearRepeat' :: (Applicative v,Foldable v, Show s, Module Int s) =>
                Int -> [Euclid v s] -> Part xs v s -> Part '[] v s
linearRepeat' number intervals part =
  unions [translate (k *^ (intervals !! k) + j *^ add intervals) part
         | i <- [negate number `div` 2..number `div` 2],
           let (j,k) = i `divMod` length intervals
         ]

linearRepeat :: (Foldable v, Show s, Module Int s,Applicative v) =>
                Int -> Euclid v s -> Part xs v s -> Part '[] v s
linearRepeat number interval part =
  unions [translate (i *^ interval) part
         | i <- [negate number `div` 2..number `div` 2]]

linearFill :: (Foldable v, Show s, RealFrac s, Floating s, Field s, Ring' s, Module Int s, Applicative v) =>
                    s -> Euclid v s -> Part xs v s -> Part '[] v s
linearFill len interval part = linearRepeat (floor (len / norm interval)) interval part

-- | Fill a rectangle in hexagonal pattern
hexagonFill :: Module Int s => RealFrac s => Floating s => Show s => Field s => Module s s
               => s -> s -> s
               -> Part2 xs s
               -> Part2 ('[ '["right"], '["back"], '["left"], '["front"],
                        '["northEast"], '["northWest"], '["southWest"],
                        '["southEast"]] :: [[Symbol]]) s
hexagonFill len width cell_size shape
  = intersection (scale' (V2 len width) square) $
    linearRepeat' no_of_rows (V2 tr_x <$> [negate tr_y, tr_y]) $
    linearFill (width + cell_size) (V2 0 cell_size) $ -- width + cell_size: we need a bit larger area because of the tr_y offsets
    shape
  where no_of_rows = floor(1.2 * len / cell_size)
        tr_x = sqrt(3)/2 * cell_size
        tr_y = cell_size / 2


--------------------------------------
-- Locations

south :: '[South] ∈ xs => RelLoc xs v a; south = getLoc @'[South]
north :: '[North] ∈ xs => RelLoc xs v a; north = getLoc @'[North]
west  :: '[West] ∈ xs => RelLoc xs v a; west = getLoc @'[West]
east  :: '[East] ∈ xs => RelLoc xs v a; east = getLoc @'[East]
nadir :: '[Nadir] ∈ xs => RelLoc xs v a; nadir = getLoc @'[Nadir]
zenith :: '[Zenith] ∈ xs => RelLoc xs v a; zenith = getLoc @'[Zenith]

southEast :: '["southEast"] ∈ xs => RelLoc xs v a; southEast = getLoc @'["southEast"]
northEast :: '["northEast"] ∈ xs => RelLoc xs v a; northEast = getLoc @'["northEast"]
southWest :: '["southWest"] ∈ xs => RelLoc xs v a; southWest = getLoc @'["southWest"]
northWest :: '["northWest"] ∈ xs => RelLoc xs v a; northWest = getLoc @'["northWest"]


projectOnPlane :: (Module scalar scalar, Field scalar) =>
                        Loc V3' scalar -> Loc V3' scalar -> Loc V3' scalar
projectOnPlane plane@Loc {locPoint = planeOrigin}
          Loc {..} = Loc {locPoint = position, locBase = locBase}
 where θ = (planeOrigin - locPoint) · planeNormal
       position = θ *^ planeNormal + locPoint
       planeNormal = locNormal plane
       -- equation : (position - planeOrigin) · planeNormal = 0

(|<-) :: (Module scalar scalar, Field scalar)
      => (t -> Loc V3' scalar) -> (t -> Loc V3' scalar) -> t -> Loc V3' scalar
(plane |<- pos) p = projectOnPlane (plane p) (pos p)
infixr |<-

projectOnLine :: (Module scalar scalar, Field scalar) =>
                       Loc V3' scalar -> Loc V3' scalar -> Loc V3' scalar
projectOnLine line@Loc {locPoint = lineOrigin}
              Loc {..} = Loc {locPoint = position, locBase = locBase}
  where cosθ = (locPoint - lineOrigin) · lineVec
        position = lineOrigin + cosθ *^ lineVec
        lineVec = locNormal line

(/<-) :: (Module scalar scalar, Field scalar) =>
               (t -> Loc V3' scalar)
               -> (t -> Loc V3' scalar) -> t -> Loc V3' scalar
(line /<- pos) p = projectOnLine (line p) (pos p)


projectOnPoint :: (Module scalar scalar, Field scalar) =>
                        Loc V3' scalar -> Loc V3' scalar -> Loc V3' scalar
projectOnPoint Loc {locPoint = lineOrigin}
              Loc {..} = projectOnLine Loc {locBase=locBase, locPoint=lineOrigin} Loc {..}

(.<-) :: (Module scalar scalar, Field scalar) =>
               (t -> Loc V3' scalar)
               -> (t -> Loc V3' scalar) -> t -> Loc V3' scalar
(line .<- pos) p = projectOnPoint (line p) (pos p)

-- yxPoint :: V2 a -> V2 a -> V2 a
-- yxPoint (V2 _ y) (V2 x _) = V2 x y

-- yxLoc :: (t -> Loc V2' a) -> (t -> Loc V2' a) -> t -> Loc V2' a
-- yxLoc f g p = Loc (yxPoint (locPoint y) (locPoint x)) (yxPoint (locBase y) (locBase x))
--   where y = f p
--         x = g p


type East = "right"
type West = "left"
type North = "back"
type South = "front"
type Zenith = "top"
type Nadir = "bottom"



-------------------------------------
-- Rendering

renderVec :: (Show a, Foldable t) => t a -> String
renderVec v = showL (map show (toList v))

showL :: [String] -> String
showL v = "[" <> intercalate ", " v <> "]"

showAngle :: Show a => Division a => Floating a => a -> String
showAngle x = show (x * (180 / pi))



