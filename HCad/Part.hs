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
module HCad.Part where

import Algebra.Linear
import Algebra.Classes
import Prelude hiding (Num(..),(/),divMod,div,recip)
import Data.Foldable
import GHC.TypeLits
import Data.List (intercalate)
import Data.Kind (Type)
import Data.Type.Equality
import Unsafe.Coerce
import Data.Char (toLower)

data SCAD = SCAD {scadPrim :: String
                 ,scadArgs :: [(String,String)]
                 ,scadBody :: [SCAD]}

data Op = Union | Intersection | Hull deriving Show
data DSC vec a where
  Polygon :: Int -> [V2 a] -> DSC V2' a
  Prim :: SCAD -> DSC vec a
  Color :: Double -> V3 s -> DSC vec s -> DSC vec s
  NOp :: Op -> [DSC vec a] -> DSC vec a
  Difference :: DSC vec a -> DSC vec a -> DSC vec a
  MultMat :: SqMat V4' a -> DSC vec a -> DSC vec a
  LExtrude :: a -> a -> a -> DSC V2' a -> DSC V3' a
  RExtrude :: Maybe Int -> a -> DSC V2' a -> DSC V3' a
  Mirror :: Euclid v a -> DSC v a -> DSC v a

type V4' = VNext V3'

difference' :: DSC vec a -> DSC vec a -> DSC vec a
difference' (Difference a b) c = Difference a (unions' [b,c])
difference' x y = Difference x y

pattern Uni :: forall vec a. [DSC vec a] -> DSC vec a
pattern Uni xs = NOp Union xs

unions' :: [DSC vec a] -> DSC vec a
unions' xs = Uni (unions'' xs)
unions'' :: [DSC vec a] -> [DSC vec a]
unions'' [] = []
unions'' (Uni xs:ys) = unions'' (xs++ys)
unions'' (x:xs) = x:unions'' xs

addOneMat :: (Ring s, Applicative v, Applicative v) => Mat s v v -> Mat s (VNext v) (VNext v)
addOneMat (Mat vs) = Mat (VNext (VNext <$> vs <*> pure zero) (VNext (pure zero) one)) 

homMat :: ScadV v => Applicative v => Functor v => Ring s => SqMat v s -> SqMat V4' s
homMat = addOneMat . conv3dMat

-- | translation as a matrix transforming homogeneous vectors
translateToMat :: (Traversable v, Ring s, Applicative v) => Euclid v s -> SqMat (VNext v) s
translateToMat (Euclid v) = Mat (VNext (VNext <$> i <*> v) (VNext (pure zero) one))
  where Mat i = identity

instance ScadV V2' where
  conv3dVec (Euclid v) = Euclid (VNext v zero)
  conv3dMat = addOneMat

instance ScadV V3' where
  conv3dVec = id
  conv3dMat = id

class (Traversable v, Applicative v) => ScadV v where
  conv3dVec :: Additive a => Euclid v a -> Euclid V3' a
  conv3dMat :: Ring a => SqMat v a -> SqMat V3' a

translate' :: ScadV vec => Traversable vec => Ring a => Applicative vec => Euclid vec a -> DSC vec a -> DSC vec a
translate' v = multmat'' (translateToMat $ conv3dVec v)

-- translate' v (Color a c t) = Color a c (translate' v t)
-- translate' v (NOp op ts) = NOp op (translate' v <$> ts)
-- translate' v (Difference t u) = Difference (translate' v t) (translate' v u)
-- translate' v (Translate v' t) = Translate (v+v') t
-- translate' v t = Translate v t

multmat' :: ScadV vec => Ring a => Traversable vec => Applicative vec => SqMat vec a -> DSC vec a -> DSC vec a
multmat' = multmat'' . homMat

multmat'' :: Ring a => Traversable vec => Applicative vec => SqMat V4' a -> DSC vec a -> DSC vec a
multmat'' v (Color a c t) = Color a c (multmat'' v t)
multmat'' v (NOp op ts) = NOp op (multmat'' v <$> ts)
multmat'' v (Difference t u) = Difference (multmat'' v t) (multmat'' v u)
multmat'' v (MultMat v' t) = MultMat (matMul v' v) t
multmat'' v t = MultMat v t
-- multmat' v (Translate v' t) = MultMat (matMul (translateToMat v') v) t

convexity :: DSC vec a -> Int
convexity = \case
  (Difference x y) -> convexity x + convexity y
  (MultMat _ r) -> convexity r
  (LExtrude _ _ _ r) -> convexity r
  (Polygon convex _) -> convex
  (Prim _) -> 2
  (Color _ _ r) -> convexity r
  (NOp Hull _) -> 2
  (NOp _ rs) -> sum (map convexity rs)
  Mirror _ r -> convexity r
  RExtrude {} -> 10
  
toSCAD :: Foldable vec => Functor vec => Floating a => Division a => Show a => DSC vec a -> SCAD
toSCAD = \case
  Mirror normal r -> SCAD "mirror" [("v",renderVec normal)] [toSCAD r]
  RExtrude fn angle partCode ->
    SCAD "rotate_extrude" ([("angle",showAngle angle)] ++ [("$fn",show x) | Just x <- [fn]]) [toSCAD partCode]
  (LExtrude  height scaleFactor twist partCode) ->
    (SCAD "linear_extrude"
      [("height",show height)
      ,("center","true")
      ,("convexity",show (convexity partCode))
      ,("scale",show scaleFactor)
      ,("twist",showAngle twist)] [toSCAD partCode])
  MultMat m r -> SCAD "multmatrix" [("m",m')] [toSCAD r]
    where m' = showL (toList (showL . toList . (fmap show) <$> fromMat m))
  Polygon _ points -> SCAD "polygon" [("points",showL (map renderVec points))] []
  Prim p -> p
  NOp op rs -> SCAD (map toLower $ show op) [] (map toSCAD rs)
  Difference r1 r2 -> SCAD "difference" [] [toSCAD r1, toSCAD r2]
  Color a c r -> SCAD "color" [("c",renderVec c),("alpha",show a)] [toSCAD r]

data Part xs vec a
  = Part {partVertices :: NamedVec xs (Euclid vec a) -- TODO: use Loc here
         ,partBases  :: NamedVec xs (SqMat vec a)
         ,partCode :: DSC vec a }

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
                  ,partCode= Prim (SCAD "import" [("file",show fname)] [])}


color' :: (Show s) => Double -> V3 s -> Part xs vec s -> Part xs vec s
color' a c Part{..} = Part {partCode = Color a c partCode
                           ,..}

color :: (Show s) => V3 s -> Part xs vec s -> Part xs vec s
color = color' 1

cube :: Show a => Ring' a => Floating a => Division a
     => Part '[ '["bottom"], '["top"], '["right"], '["back"],
                        '["left"], '["front"], '["northEast"], '["northWest"],
                        '["southWest"], '["southEast"]] V3' a
cube = extrude one square

sphere :: Part3 '[] a
sphere = Part {partVertices = Nil, partBases = Nil
              ,partCode = Prim (SCAD "sphere" [("r","0.5")] [])}

square :: forall a. Module a a => Floating a => Show a => Ring a
       => Part2 (SimpleFields '[East, North, West, South, "northEast", "northWest", "southWest", "southEast"]) a
square = Part {partVertices = matVecMul <$> partBases <*> (V2 <$> scales <*> pure 0)
              ,partCode = Prim (SCAD "square" [("size","1"),("center","true")] [])
              ,..}
  where partBases = rotation2d <$> angles
        scales = 0.5 :* 0.5 :* 0.5 :* 0.5 :* sqrt 0.5 :* sqrt 0.5 :* sqrt 0.5 :* sqrt 0.5 :* Nil
        angles = (pi *) <$> (0   :* 0.5 :* 1   :* 1.5 :* 0.25     :* 0.75     :* 1.25     :* 1.75     :* Nil)

rectangle :: (Field s, Show s, Module s s, Floating s) =>
                   Euclid V2' s
                   -> Part
                        '[ '["right"], '["back"], '["left"], '["front"], '["northEast"],
                          '["northWest"], '["southWest"], '["southEast"]]
                        V2'
                        s
rectangle sz = scale' sz  square

circle :: Part2 '[] a
circle = Part {partVertices = Nil, partBases = Nil
              ,partCode = Prim (SCAD "circle" [("r","0.5")] [])}

polygon' :: Show a => Int -> [V2 a] -> Part2 '[] a
polygon' convex points
  = Part {partVertices = Nil
         ,partBases = Nil
         ,partCode = Polygon convex points}

polygon :: Show a => [V2 a] -> Part2 '[] a
polygon = polygon' 2


extrude :: forall a xs. Division a => Floating a => Module a a => Fractional a => Show a
              => a -> Part2 xs a -> Part3 (SimpleFields '[Nadir,Zenith] ++ xs) a
extrude height p = extrudeEx height 1 0 p


extrudeEx :: forall a xs. Floating a => Division a => Module a a => Fractional a => Show a
              => a -> a -> a -> Part2 xs a -> Part3 (SimpleFields '[Nadir,Zenith] ++ xs) a
extrudeEx height scaleFactor twist Part{..}
  = Part {partVertices = (flip matVecMul (V3 0 0 (0.5 * height)) <$> botTopBases) ++* (z0 <$> partVertices)
         ,partBases = botTopBases ++* (conv  <$> partBases)
         ,partCode = LExtrude height scaleFactor twist partCode
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
lathe = latheEx Nothing (2*pi)

latheEx :: (Show a, Division a, Floating a) => Maybe Int -> a -> Part2 xs a -> Part3 '[] a
latheEx fn angle Part{..} =
  Part {partVertices = Nil,
        partBases = Nil,
        partCode = RExtrude fn angle partCode
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
                  ,partCode = unions' [partCode p1,partCode p2]}
union :: Part ys v a -> Part xs v a -> Part (xs ++ ys) v a
union = flip (/+)

unions :: [Part xs v a] -> Part '[] v a
unions ps = Part {partVertices = Nil
                 ,partBases = Nil
                 ,partCode = unions' (map partCode ps)}

intersection :: Part ys v a -> Part xs v a -> Part (xs ++ ys) v a
intersection p2 p1 = Part {partVertices = partVertices p1 ++* partVertices p2
                          ,partBases = partBases p1 ++* partBases p2
                          ,partCode = NOp Intersection [partCode p1,partCode p2]}

hull :: Part ys v a -> Part xs v a -> Part (xs ++ ys) v a
hull p2 p1 = Part {partVertices = partVertices p1 ++* partVertices p2
                          ,partBases = partBases p1 ++* partBases p2
                          ,partCode = NOp Hull [partCode p1,partCode p2]}

(/-) :: Part xs v a -> Part ys v a -> Part (xs ++ ys) v a
(/-) p1 p2 = Part {partVertices = partVertices p1 ++* partVertices p2
                   ,partBases = partBases p1 ++* partBases p2
                   ,partCode = difference' (partCode p1) (partCode p2)}



difference :: Part ys v a -> Part xs v a -> Part (xs ++ ys) v a
difference = flip (/-)

translate :: forall (v :: Type -> Type) s xs. ScadV v => Ring s => Traversable v => Additive s => Applicative v => Foldable v => Show s =>  Euclid v s -> Part xs v s -> Part xs v s
translate v Part{..} = Part {partBases = partBases
                            ,partVertices = (v +) <$> partVertices
                            ,partCode = translate' v partCode
                            }

rotate :: ScadV v => Traversable v => Applicative v => Show s => Floating s => Division s => Module s s => Ring s => SqMat v s -> Part xs v s -> Part xs v s
rotate m Part{..} = Part {partVertices = matVecMul m <$> partVertices
                         ,partBases = (\subBase -> m `matMul`subBase ) <$>  partBases
                         ,partCode = multmat' m partCode}


mirror :: forall a v xs. Applicative v => Field a => Ring' a => Foldable v => Show a => Euclid v a -> Part xs v a -> Part xs v a
mirror normal Part{..}
  = Part {partBases = mm <$> partBases
         ,partVertices = m <$> partVertices
         ,partCode = Mirror normal partCode}
    where m :: Euclid v a -> Euclid v a
          m x = x - (fromInteger 2 * d) *^ normal
            where d = normal · x
          m' :: v a -> v a
          m' = fromEuclid . m . Euclid
          mm :: SqMat v a -> SqMat v a
          mm = Mat . fmap m' . fromMat

scale' :: ScadV v => (Field s,Show s) => Euclid v s -> Part xs v s -> Part xs v s
scale' v Part{..} = Part {partBases = partBases -- FIXME: shear the base!
                         ,partVertices = (v ⊙) <$> partVertices
                         ,partCode = multmat' (diagonal v) partCode }

scale :: (ScadV v, Field s, Show s) => s -> Part xs v s -> Part xs v s
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
at :: Ring s => (ScadV v, Show s) => (RelLoc xs v s) -> (Part xs v s -> Part ys v s) -> (Part xs v s -> Part ys v s)
at relLoc f body = translating (locPoint (relLoc body)) f body

translating :: ScadV v => Ring s => Show s =>
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


rotating :: ScadV v => (Show s, Floating s, Field s, Module s s) =>
                      SqMat v s
                      -> (Part xs1 v s -> Part xs2 v s)
                      -> Part xs1 v s
                      -> Part xs2 v s
rotating o f = rotate o . f . rotate (transpose o)

-- | Put the focus point on the given locus
on :: ScadV v => Division a => Module a a => Floating a => Field a => Show a
   => RelLoc xs v a -> (Part xs v a -> Part ys v a) -> (Part xs v a -> Part ys v a)
on relLoc f body = translating locPoint (rotating locBase f) body
  where Loc{..} = relLoc body

-- | Center the given location
center :: ScadV v => Ring a => Show a => RelLoc xs v a -> Part xs v a -> Part xs v a
center getX p = translate (negate (locPoint (getX p))) p



------------------------------------------------
-- Non-primitive ops

rotate2d :: (Show s, Floating s, Division s, Module s s) =>
                  s -> Part xs V2' s -> Part xs V2' s
rotate2d angle = rotate (rotation2d angle)

xAxis :: V3 Double
xAxis = V3 1 0 0

yAxis :: V3 Double
yAxis = V3 0 1 0

zAxis :: V3 Double
zAxis = V3 0 0 1


mirrored :: forall v a xs. Module a a => Field a => Applicative v => (Foldable v, Show a) => Euclid v a -> Part xs v a -> Part xs v a
mirrored axis part = unitR @xs #> union (forget $ mirror axis part) part


-- | Regular polygon contained a unit-diameter circle.
regularPolygon :: Field a => Module a a => Division a => Floating a => Show a => Int -> Part2 '[] a
regularPolygon order = scale 0.5 (polygon coords)
  where coords=[V2 (cos th) (sin th)
               | i <- [0..order-1],
                 let th = fromIntegral i*(2.0*pi/fromIntegral order) ];

-- | Regular polygon containing a unit-diameter circle.
regularPolygonO :: Field a => Module a a => Division a => Floating a => Show a => Int -> Part2 '[] a
regularPolygonO order = scale (1 / cos (pi / fromIntegral order)) $ regularPolygon order

-- | A circle with an angular top. The argument is the top angle; often pi/2 or pi/3
waterdrop :: Field a => (Division a, Group a, Floating a, Show a) => a -> Part2 '[] a
waterdrop alpha = union circle (scale 0.5 $ polygon [V2 c s, V2 0 (1/s), V2 (-c) s])
  where s = sin alpha
        c = cos alpha


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

linearRepeat' :: ScadV v => Ring s => (Show s, Module Int s) =>
                Int -> [Euclid v s] -> Part xs v s -> Part '[] v s
linearRepeat' number intervals part =
  unions [translate (k *^ (intervals !! k) + j *^ add intervals) part
         | i <- [negate number `div` 2..number `div` 2],
           let (j,k) = i `divMod` length intervals
         ]

linearRepeat :: ScadV v => Ring s => (Show s, Module Int s) =>
                Int -> Euclid v s -> Part xs v s -> Part '[] v s
linearRepeat number interval part =
  unions [translate (i *^ interval) part
         | i <- [negate number `div` 2..number `div` 2]]

linearFill :: (ScadV v, Show s, RealFrac s, Floating s, Field s, Ring' s, Module Int s) =>
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

