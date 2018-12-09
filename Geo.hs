module Geo where

import Data.Complex


-- Reals

type R = Double

inf :: R
inf = read "Infinity"

sqr :: (Num a) => a -> a
sqr x = x * x

extremes :: [R] -> (R, R)
extremes [] = error "Can't calculate segment box for an empty list"
extremes (v:vs) = foldl extremesAppend (v, v) vs
  where extremesAppend accu pivot =
          let (min, max) = accu
              newAccu | pivot < min = (pivot, max)
                      | pivot > max = (min, pivot)
                      | otherwise   = accu
          in newAccu

-- Vectors

type V = Complex R

v::R -> R -> V
v a b = a :+ b

x::V -> R
x (a :+ _) = a
y::V -> R
y (_ :+ b) = b

scl s (x :+ y) = (s * x) :+ (s * y)

norm2 (x :+ y) = x * x + y * y
norm = sqrt . norm2

dist2 c0 c1 = norm2 $ c0 - c1

dist :: V -> V -> R
dist a b = sqrt $ dist2 a b

crossProduct (x0 :+ y0) (x1 :+ y1) = x0*y1 - x1*y0

-- Orientations

data Orientation = X | Y
  deriving (Show, Eq)


-- Lines

data Line = Line V V
  deriving (Show)


-- Boxes

data Box = Box V V
  deriving Show

fullPlaneBox = Box ((-inf) :+ (-inf)) (inf :+ inf)

box :: [V] -> Box
box vs = let (x0, x1) = extremes $ map x vs
             (y0, y1) = extremes $ map y vs
         in Box (v x0 y0) (v x1 y1)

boxDx (Box (x0 :+ _) (x1 :+ _)) = x1 - x0
boxDy (Box (_ :+ y0) (_ :+ y1)) = y1 - y0

boxArea (Box v0 v1) = (x d) * (y d)
  where d = v1 - v0

recoordinateBox :: V -> V -> Box -> Box
recoordinateBox o (xu :+ yu) (Box p0 p1) =
  let (x0 :+ y0) = p0 - o
      (x1 :+ y1) = p1 - o
  in if (x0 == -inf) || (x1 == inf) || (y0 == -inf) || (y1 == inf)
     then (Box ((-inf) :+ (-inf)) (inf :+ inf))
     else let idet = 1.0/(xu * xu + yu * yu)
              ps = map (\(x,y) -> (x * xu + y * yu, y * xu - x * yu)) [(x0, y0), (x0, y1), (x1, y1), (x1, y0)]
              (x0', x1') = extremes $ map fst ps
              (y0', y1') = extremes $ map snd ps
          in Box (scl idet $ x0' :+ y0') (scl idet $ x1' :+ y1')

divideBox :: Box -> Orientation -> R -> (Box, Box)
divideBox (Box (x0 :+ y0) (x1 :+ y1)) orientation pivot =
  case orientation of
    X -> (Box (x0 :+ y0) (pivot :+ y1), Box (pivot :+ y0) (x1 :+ y1))
    Y -> (Box (x0 :+ y0) (x1 :+ pivot), Box (x0 :+ pivot) (x1 :+ y1))


-- Circles

data Circle = Circle { getCenter::V, getRadius::R }
  deriving (Show, Eq)

instance Ord Circle where
  compare (Circle (x0 :+ y0) r0) (Circle (x1 :+ y1) r1) =
    (compare r0 r1) `mappend` (compare x0 x1) `mappend` (compare y0 y1)

recoordinateCircle :: V -> V -> Circle -> Circle
recoordinateCircle o (xu :+ yu) (Circle c r) =
  let (xc :+ yc) = c - o
      idet = 1.0/(xu * xu + yu * yu)
      xc' = idet * (xc * xu + yc * yu)
      yc' = idet * (yc * xu - xc * yu)
      r' = r / sqrt idet
  in Circle (xc' :+ yc') r'

scaleCircle :: R -> Circle -> Circle
scaleCircle s (Circle c r) = Circle (scl s c) (s * r)

circleBoxIntersectionBox :: Box -> Circle -> Box
circleBoxIntersectionBox (Box p0 p1) (Circle c r) =
  let (x0 :+ y0) = p0 - c
      (x1 :+ y1) = p1 - c
      r_2 = r * r
      x_2 = r_2 - (sqr (if y0 > 0 then y0 else if y1 < 0 then y1 else 0))
      y_2 = r_2 - (sqr (if x0 > 0 then x0 else if x1 < 0 then x1 else 0))
  in if (x_2 < 0) || (y_2 < 0)
     then (Box p0 p0) -- Intersection is empty
     else let x = sqrt x_2
              y = sqrt y_2
              x0' = max (-x) x0
              y0' = max (-y) y0
              x1' = min x x1
              y1' = min y y1
          in Box ((x0' :+ y0') + c) ((x1' :+ y1') + c)

