import Data.Complex
import Data.List (partition)
import System.Random
import Debug.Trace
import qualified Data.PQueue.Prio.Min as Q

type R = Double

type V = Complex R

v::R -> R -> V
v a b = a :+ b

x::V -> R
x (a :+ _) = a
y::V -> R
y (_ :+ b) = b

data Circle = Circle { center::V, radius::R }
  deriving (Show, Eq)

data Orientation = X | Y
  deriving (Show, Eq)

data Tree = Tree { orientation::Orientation,
                   pivot::R,
                   first::Tree,
                   second::Tree }
          | Tail [Circle]
  deriving Show

data Box = Box V V
  deriving Show

-- freeArea (Tree _ _ _ a) = a
-- freeArea (Tail _ a) = a
-- data Top = Top Box Tree
-- newTop = let box = (Box 0 (1 :+ 1))
--         in Top box (Tail [] (boxArea box))

extremes::[R] -> (R, R)
extremes [] = error "Can't calculate segment box for an empty list"
extremes (v:vs) = foldl extremesAppend (v, v) vs
  where extremesAppend accu pivot =
          let (min, max) = accu
              newAccu | pivot < min = (pivot, max)
                      | pivot > max = (min, pivot)
                      | otherwise   = accu
          in newAccu

box :: [V] -> Box
box vs = let (x0, x1) = extremes $ map x vs
             (y0, y1) = extremes $ map y vs
         in Box (v x0 y0) (v x1 y1)

inf = read "Infinity"

fullPlaneBox = Box ((-inf) :+ (-inf)) (inf :+ inf)

maxTailSize = 3

orientationF X = x
orientationF Y = y

insertMany :: Tree -> [Circle] -> Tree
insertMany = foldl insert

insert :: Tree -> Circle -> Tree
insert tree circle@(Circle c r) =
  case tree of
    Tail circles -> newTree (circle:circles)
    Tree orientation pivot first second ->
      let f = orientationF orientation
          newFirst  = if (((f c) - r) <= pivot)
                      then first
                      else insert first circle
          newSecond = if (((f c) + r) >= pivot)
                      then second
                      else insert second circle
      in Tree orientation pivot newFirst newSecond

emptyTree = Tail []

newTree :: [Circle] -> Tree
newTree circles = if (length circles) <= maxTailSize
                  then Tail circles
                  else let (x0, x1) = extremes $ map (x . center) circles
                           (y0, y1) = extremes $ map (y . center) circles
                           dx = x1 - x0
                           dy = y1 - y0
                           (orientation, p) = if (dx > dy)
                                              then (X, 0.5 * (x0 + x1))
                                              else (Y, 0.5 * (y0 + y1))
                       in insertMany (Tree orientation p emptyTree emptyTree) circles

randomCircle :: [R] -> (Circle, [R])
randomCircle (x : y : r : rs) = ((Circle (v x y) (0.2 * r)), rs)
randomCircles rs = let (circle, rs') = randomCircle rs
                   in circle:(randomCircles rs')

main = do
  gen <- getStdGen
  putStrLn $ show $ newTree $ take 100 $ randomCircles $ ((randoms gen)::[R])

dist2 c0 c1 = realPart (dc * (conjugate dc))
  where dc = c0 - c1

dist :: V -> V -> R
dist = dist2 . sqrt

pointBoxDist2 :: V -> Box -> R
pointBoxDist2 (x :+ y) (Box (x0 :+ y0) (x1 :+ y1)) =
  let bestX = if x >= x1 then x1 else if x <= x0 then x0 else x
      bestY = if y >= y1 then y1 else if y <= y0 then y0 else y
      dx = bestX - x
      dy = bestY - y
  in dx * dx + dy * dy

pointBoxDist :: V -> Box -> R
pointBoxDist p box = sqrt $ pointBoxDist2 p box

map2 _ [] = []
map2 f (a1:a2:as) = (f a1 a2):(map2 f as)
map3 _ [] = []
map3 f (a1:a2:a3:as) = (f a1 a2 a3):(map3 f as)

boxArea (Box v0 v1) = (x d) * (y d)
  where d = v1 - v0

newCircle x0 y0 r = Circle (v x0 y0) r

circleDist (Circle p0 r0) (Circle p1 r1) = max 0 ((dist p0 p1) - r0 - r1)

pointCircleDist p0 (Circle p1 r1) = max 0 ((dist p0 p1) - r1)

pointCircleDist2 p circle = sqr $ pointCircleDist p circle
  where sqr n = n * n

divideBox :: Box -> Orientation -> R -> (Box, Box)
divideBox (Box (x0 :+ y0) (x1 :+ y1)) orientation pivot =
  case orientation of
    X -> (Box (x0 :+ y0) (pivot :+ y1), Box (pivot :+ y0) (x1 :+ y1))
    Y -> (Box (x0 :+ y0) (x1 :+ pivot), Box (x0 :+ pivot) (x1 :+ y1))

nearestCircle :: Tree -> V -> (Maybe Circle, R)
nearestCircle tree p = let box = fullPlaneBox
                           queue = (Q.singleton inf (box, tree))::(Q.MinPQueue R (Box, Tree))
                       in nearestInQueue queue p (Nothing, inf)
  where nearestInQueue queue p old@(best,minD2) =
          case (Q.getMin queue) of
            Nothing -> (best, minD2)
            Just (d2, (box, tree)) ->
              if d2 > minD2
              then (best, minD2)
              else let queue' = Q.deleteMin queue
                   in case tree of
                        Tail circles -> nearestInQueue queue' p $ nearestInList circles p old
                        Tree orientation pivot first second ->
                          let (firstBox, secondBox) = divideBox box orientation pivot
                              firstD2 = pointBoxDist2 p firstBox
                              secondD2 = pointBoxDist2 p secondBox
                              queue'' = Q.insert firstD2 (firstBox, first) queue'
                              queue''' = Q.insert secondD2 (secondBox, second) queue''
                          in nearestInQueue queue''' p old
        nearestInList list p old = foldl pickBest old list
        pickBest old@(_, minD2) circle = let d2 = pointCircleDist2 p circle
                                         in if d2 < minD2
                                            then (Just circle, d2)
                                            else old

tr :: (Show a) => String -> a -> a
tr s a = trace (s ++ ": " ++ (show a)) a

boxCircleIntersectionArea (Box (x0' :+ y0') (x1' :+ y1')) (Circle (xc :+ yc) r) =

  let x0 = max (-r) (x0' - xc)
      y0 = max (-r) (y0' - yc)
      x1 = min   r  (x1' - xc)
      y1 = min   r  (y1' - yc)
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
                      dq = q1 - q0
                  in s0 + s1 + 0.5 * (cross q0 dq)

  where sector0Area r p0 p1 = r * (phase $ p0 * (conjugate p1))
        scl s (x :+ y) = (s * x) :+ (s * y)
        cross (x0 :+ y0) (x1 :+ y1) = x0 * y1 - x1 * y0
