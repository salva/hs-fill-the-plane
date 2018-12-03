module Metrics ( Metric
               , AdjacentCircleRadiusMetric
               , DistToPointMetric
               , boxDist
               , circleDist)
where

import Data.Complex
import Geo

-- Metrics

class Metric t where
  boxDist :: t -> Box -> R
  circleDist :: t -> Circle -> R


-- Distance functions

circlesDist (Circle p0 r0) (Circle p1 r1) = max 0 ((dist p0 p1) - r0 - r1)

pointBoxDist2 :: V -> Box -> R
pointBoxDist2 (x :+ y) (Box (x0 :+ y0) (x1 :+ y1)) =
  let bestX = if x >= x1 then x1 else if x <= x0 then x0 else x
      bestY = if y >= y1 then y1 else if y <= y0 then y0 else y
      dx = bestX - x
      dy = bestY - y
  in dx * dx + dy * dy

pointBoxDist :: V -> Box -> R
pointBoxDist p box = sqrt $ pointBoxDist2 p box

pointCircleDist :: V -> Circle -> R
pointCircleDist p0 (Circle p1 r1) = max 0 ((dist p0 p1) - r1)

pointCircleDist2 :: V -> Circle -> R
pointCircleDist2 p circle = sqr $ pointCircleDist p circle

circleTouchesBox :: Circle -> Box -> Bool
circleTouchesBox (Circle c r) box = (pointBoxDist2 c box) < r * r


-- Distance to point metric

newtype DistToPointMetric = DistToPointMetric { getV :: Complex Double }
  deriving Show

instance Metric DistToPointMetric where
  boxDist m box = pointBoxDist2 (getV m) box
  circleDist m circle = pointCircleDist2 (getV m) circle


-- Distance to circle metric

data AdjacentCircleRadiusMetric = AdjacentCircleRadiusMetric V V V

instance Metric AdjacentCircleRadiusMetric where
  boxDist (AdjacentCircleRadiusMetric c o v) b =
    let (Box (x0 :+ y0) (x1 :+ y1)) = recoordinateBox o v b
    in if x1 <= 0
       then inf
       else
         let y = if y1 <= 0 then -y1 else if y0 >= 0 then y0 else 0
             y_2 = y * y
             r1 = 0.5 * (y_2/x1 + x1)
         in if (x1 <= r1) && (x1 < inf)
            then r1
            else if x0 <= y
                 then y
                 else 0.5 * (y_2/x0 + x0)

  circleDist (AdjacentCircleRadiusMetric c o v) (circle@(Circle c1 _)) =
    if c == c1
    then inf
    else let (Circle (xc :+ yc) rc) = recoordinateCircle o v circle
             xc_rc = xc + rc
         in if xc_rc < 0
            then inf
            else 0.5 * (yc * yc / xc_rc + xc - rc)

