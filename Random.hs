module Random where

import Data.Complex
import System.Random

import Tree
import Geo
import Metrics

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
                in insertRandomCircles (n - 1) tree1 g1

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
  let (nearest1, _) = nearestCircle tree (DistToPointMetric p) (Nothing, r * r)
  in case nearest1 of
       Nothing -> Circle p r
       Just (Circle c1 r1) ->
         let v = if p == c1 then 1 else p - c1
             v1 = scl (1.0 / magnitude v) v
             o = c1 + (scl r1 v1)
             (nearest2, m2) = nearestCircle tree (AdjacentCircleRadiusMetric c1 o v1) (Nothing, r)
         in case nearest2 of
              Nothing -> Circle (o + scl r v1) r
              Just _ -> Circle (o + scl m2 v1) m2
