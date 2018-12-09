module Random where

import Data.Complex
import System.Random
import Debug.Trace (trace)

import Tree
import Geo
import Metrics
import Apollonius

tr :: Show t => String -> t -> t
-- tr a b = trace (a ++ ": " ++ (show b)) b
tr _ b = b

tr2 :: Show t => String -> t -> t
tr2 a b = trace (a ++ ": " ++ (show b)) b

randomTreeIO n = do
    g0 <- getStdGen
    let (tree, g1) = randomTree n g0
    return tree

randomTree :: (RandomGen g) => Int -> g -> (Tree, g)

randomTree n g =
  insertRandomCircles n emptyTree g

insertRandomCircles :: (RandomGen g) => Int -> Tree -> g -> (Tree, g)
insertRandomCircles n tree0 g0
  | n == 0    = (tree0, g0)
  | otherwise = let (tree1, g1) = insertRandomCircle tree0 g0
                in insertRandomCircles (tr2 "n" $ n - 1) tree1 g1

insertRandomCircle :: (RandomGen g) => Tree -> g -> (Tree, g)
insertRandomCircle tree0 g0 =
  let (circle, g1) = randomCircle tree0 g0
      tree1 = insert tree0 fullPlaneBox circle
  in (tree1, g1)

defaultBox = Box 0 (1 :+ 1)

randomPointInBox (Box (x0 :+ y0) (x1 :+ y1)) g0 =
  let (x, g1) = randomR (x0, x1) g0
      (y, g2) = randomR (y0, y1) g1
  in (x :+ y, g2)

randomCircle :: (RandomGen g) => Tree -> g -> (Circle, g)
randomCircle tree g0 =
  let (c, g1) = randomPointInBox defaultBox g0
      (r, g2) = randomR (0, 0.5) g1
      circle = adjustedCircle tree c r
  in (circle, g2)

adjustedCircle tree p r =
  tr "adjustedCircle" $
  let (nearest1, _) = tr "(nearest1, m1)" $ nearestCircle tree (DistToPointMetric p) (Nothing, r * r)
  in case nearest1 of
       Nothing -> Circle p r
       Just circle1@(Circle c1 r1) ->
         let v = if p == c1 then 1 else p - c1
             v1 = scl (1.0 / magnitude v) v
             o = c1 + (scl r1 v1)
             (nearest2, m2) = tr "(nearest2, m2)" $ nearestCircle tree (AdjacentCircleRadiusMetric c1 o v1) (Nothing, r)
         in case nearest2 of
              Nothing -> Circle (o + scl r v1) r
              Just circle2 ->
                let (nearest3, m3) = tr "(nearest3, m3)" $ nearestCircle tree (ApolloniusRadiusMetric circle1 circle2 o) (Nothing, r)
                    in case nearest3 of
                         Nothing -> Circle (o + scl m2 v1) m2
                         Just circle3 ->
                           let (f:fs) = tr "apollonius" $ apollonius circle1 circle2 circle3
                           in foldl smallerCircle f fs
                           where smallerCircle circle1 circle2 = if (getRadius circle1) <= (getRadius circle2)
                                                                 then circle1
                                                                 else circle2
