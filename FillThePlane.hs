
import Data.Complex
import Data.List (partition)
-- import System.Random
import Debug.Trace

type R = Double

type V = Complex R

v::R -> R -> V
v a b = a :+ b

x::V -> R
x (a :+ _) = a
y::V -> R
y (_ :+ b) = b

data Divider = X R | Y R
  deriving Show

data Tree = Tree Divider Tree Tree | Tail [V]
  deriving Show

before::Divider -> V -> Bool
before (X a) v = if a < (x v)
  then True
  else False

before(Y a) v = if a < (y v)
  then True
  else False

maxTailSize = 3

insert::Tree -> V -> Tree
insert tree v = case tree of
  Tail a -> newTree (v:a)
  Tree div first second ->
    case (before div v) of
      True -> Tree div (insert first v) second
      False -> Tree div first (insert second v)

newTree vs = if (length vs) > maxTailSize
  then divideTree vs
  else Tail vs

divideTree vs = let (x0, x1) = interval $ map x vs
                    (y0, y1) = interval $ map y vs
                    dx = x1 - x0
                    dy = y1 - y0
                    div = if (dx > dy)
                      then X (0.5 * (x0 + x1))
                      else Y (0.5 * (y0 + y1))
                    (first, second) = partition (before div) vs
                in Tree div (newTree first) (newTree second)

interval::[R] -> (R, R)
interval [] = error "Can't calculate segment box for an empty list"
interval (v:vs) = foldl intervalAppend (v, v) vs
  where intervalAppend accu pivot =
          let (min, max) = accu
              newAccu | pivot < min = (pivot, max)
                      | pivot > max = (min, pivot)
                      | otherwise   = accu
          in newAccu

dist2 c0 c1 = realPart (dc * (conjugate dc))
  where dc = c0 - c1

dist p0 p1 = sqrt (dist2 p0 p1)

map2 _ [] = []
map2 f (a1:a2:as) = (f a1 a2):(map2 f as)
map3 _ [] = []
map3 f (a1:a2:a3:as) = (f a1 a2 a3):(map3 f as)

data Box = Box V V

boxArea (Box v0 v1) = (x d) * (y d)
  where d = v1 - v0


data Circle = Circle V R
  deriving Show

newCircle x0 y0 r = Circle (v x0 y0) r

circleDist (Circle p0 r0) (Circle p1 r1) = max 0 ((dist p0 p1) - r0 - r1)

pointCircleDist p0 (Circle p1 r1) = max 0 ((dist p0 p1) - r1)

tr :: (Show a) => String -> a -> a
tr s a = trace (s ++ ": " ++ (show a)) a

boxCircleIntersectionArea (Box (x0' :+ y0') (x1' :+ y1')) (Circle (xc :+ yc) r) =

  let x0 = x0' - xc
      y0 = y0' - yc
      x1 = x1' - xc
      y1 = y1' - yc
  in if (x0 >=  r) ||
        (x1 <= -r) ||
        (y0 >=  r) ||
        (y1 <= -r)
     then 0.0
     else let p0  = v x0 y0
              p01 = v x0 y1
              p1  = v x1 y1
              p10 = v x1 y0
          in
            (segmentCircle0IntersectionArea p0  p01 r) +
            (segmentCircle0IntersectionArea p01 p1  r) +
            (segmentCircle0IntersectionArea p1  p10 r) +
            (segmentCircle0IntersectionArea p10 p0  r)

scl s (x :+ y) = (s * x) :+ (s * y)

versor v0 = scl (1.0 / (magnitude v0)) v0

cross (x0 :+ y0) (x1 :+ y1) = x0 * y1 - x1 * y0

segmentCircle0IntersectionArea p0@(x0 :+ y0) p1@(x1 :+ y1) r =
  let x0_2 = x0 * x0
      x1_2 = x1 * x1
      y0_2 = y0 * y0
      y1_2 = y1 * y1
      x0x1 = x0 * x1
      y0y1 = y0 * y1
      x0_2_y0_2 = x0_2 + y0_2
      x0x1_y0y1 = x0x1 + y0y1
      a  = x0_2_y0_2 + x1_2 + y1_2 - 2 * x0x1_y0y1
      b' = x0_2_y0_2 - x0x1_y0y1
      c  = x0_2_y0_2 - r * r
      d = b' * b' - a * c
  in if d <= 0
     then sector0Area r p0 p1
     else let dp = p1 - p0
              sqrt_d = sqrt d
              alfa0' = (b' + sqrt_d) / a
              alfa1' = (b' - sqrt_d) / a
              (alfa0, alfa1) = if alfa0' < alfa1' then (alfa0', alfa1') else (alfa1', alfa0')
          in if (alfa0 >= 1.0) || (alfa1 <= 0.0)
             then sector0Area r p0 p1
             else let (s0, q0) = if alfa0 <= 0
                                 then (0, p0)
                                 else let dp0 = scl alfa0 dp
                                          q0' = p0 + dp0
                                      in (sector0Area r p0 q0', q0')
                      (s1, q1) = if alfa1 >= 1
                                 then (0, p1)
                                 else let dp1 = scl (1 - alfa1) dp
                                          q1' = p1 - dp1
                                      in (sector0Area r q1' p1, q1')
                  in s0 + s1 + 0.5 * (cross q0 (q1 - q0))

  where sector0Area r p0 p1 = let v0 = versor p0
                                  v1 = versor p1
                                  angle = v0 / v1
                              in r * (phase angle)
