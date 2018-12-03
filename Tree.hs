module Tree where

import Data.Complex
import Data.List (partition, sort)
import qualified Data.PQueue.Prio.Min as Q
import Geo
import Metrics

maxTailSize = 3

data Tree = Tree { orientation::Orientation,
                   pivot::R,
                   first::Tree,
                   second::Tree }
          | Tail [Circle]
  deriving Show

emptyTree = Tail []

treeCirclesWithDups tree = appendFrom tree []
  where appendFrom (Tail circles) tail = circles ++ tail
        appendFrom (Tree _ _ first second) tail = appendFrom first (appendFrom second tail)

insertMany :: Tree -> [Circle] -> Tree
insertMany tree0 [] = tree0
insertMany tree0 (c:cs) = insertMany (insert tree0 fullPlaneBox c) cs

insert :: Tree -> Box -> Circle -> Tree
insert tree box circle =
  case tree of
    Tail circles -> newTreeWithBox box (circle:circles)
    Tree orientation pivot first second ->
      let (firstBox, secondBox) = divideBox box orientation pivot
          newFirst  = if circleTouchesBox circle firstBox
                      then insert first firstBox circle
                      else first
          newSecond = if circleTouchesBox circle secondBox
                      then insert second secondBox circle
                      else second
      in Tree orientation pivot newFirst newSecond

bestPivot1D :: (V -> R) -> [Box] -> (Maybe R, Int)
bestPivot1D f boxes =
  let n = length boxes
      borders = sort $ boxes >>= (\(Box p0 p1) -> [(f p0, 0, 1), (f p1, 1, 0)])
      entropy0 = 2 * n * n
      (_, _, _, _, bestL, bestEntropy) = foldl evaluate (0, n, -inf, 2, Nothing, entropy0) borders
        where evaluate (args@(before, after, lastL, lastEnd, bestL, bestEntropy)) (border@(nextL, nextEnd, nextStart)) =
                let entropy = (sqr $ 2 * before - n) + (sqr $ 2 * after - n)
                in if (lastEnd /= nextEnd) && (entropy < bestEntropy)
                   then (before + nextStart, after - nextEnd, nextL, nextEnd, Just (0.5 * (lastL + nextL)), entropy)
                   else (before + nextStart, after - nextEnd, nextL, nextEnd, bestL, bestEntropy)
  in (bestL, bestEntropy)

bestPivot box circles =
  let boxes = map (circleBoxIntersectionBox box) circles
      (bestX, bestEntropyX) = bestPivot1D x boxes
      (bestY, bestEntropyY) = bestPivot1D y boxes
  in case bestX of
       Nothing -> case bestY of
                    Nothing -> Nothing
                    Just y  -> Just (Y, y)
       Just x  -> case bestY of
                    Nothing -> Just (X, x)
                    Just y  -> let (Box (x0 :+ y0) (x1 :+ y1)) = box
                                   dx = x1 - x0
                                   dy = y1 - y0
                                   (balancedEntropyX, balancedEntropyY) =
                                     if dx == dy -- handle infinity
                                     then (fromIntegral bestEntropyX, fromIntegral bestEntropyY)
                                     else (dy * fromIntegral bestEntropyX, dx * fromIntegral bestEntropyY)
                               in Just $ if balancedEntropyX < balancedEntropyY then (X, x) else (Y, y)

newTreeWithBox box circles = if (length circles) <= maxTailSize
                             then Tail circles
                             else case bestPivot box circles of
                                    Nothing -> Tail circles
                                    Just (orientation, pivot) ->
                                      let (firstBox, secondBox) = divideBox box orientation pivot
                                          firstCircles  = [c | c <- circles, circleTouchesBox c firstBox]
                                          secondCircles = [c | c <- circles, circleTouchesBox c secondBox]
                                      in Tree orientation pivot (newTreeWithBox firstBox firstCircles) (newTreeWithBox secondBox secondCircles)

newTree :: [Circle] -> Tree
newTree = newTreeWithBox fullPlaneBox

nearestCircleInList :: (Metric m) => [Circle] -> m -> (Maybe Circle, R) -> (Maybe Circle, R)
nearestCircleInList list m old = foldl pickBest old list
  where pickBest old@(_, minD) circle = let d = circleDist m circle
                                        in if d < minD
                                           then (Just circle, d)
                                           else old


nearestCircle :: (Metric m) => Tree -> m -> (Maybe Circle, R) -> (Maybe Circle, R)
nearestCircle tree m (bestSoFar, d) = let box = fullPlaneBox
                                          queue = (Q.singleton 0 (box, tree))::(Q.MinPQueue R (Box, Tree))
                                      in nearestInQueue queue m (bestSoFar, d)
  where nearestInQueue queue m old@(bestSoFar,minD) =
          case (Q.getMin queue) of
            Nothing -> (bestSoFar, minD)
            Just (d, (box, tree)) ->
              if d > minD
              then old
              else let queue' = Q.deleteMin queue
                   in case tree of
                        Tail circles -> nearestInQueue queue' m $ nearestCircleInList circles m old
                        Tree orientation pivot first second ->
                          let (firstBox, secondBox) = divideBox box orientation pivot
                              firstD = boxDist m firstBox
                              secondD = boxDist m secondBox
                              queue'' = Q.insert firstD (firstBox, first) queue'
                              queue''' = Q.insert secondD (secondBox, second) queue''
                          in nearestInQueue queue''' m old

nearestCircleBruteForce :: (Metric m) => Tree -> m -> (Maybe Circle, R) -> (Maybe Circle, R)
nearestCircleBruteForce tree m old = case tree of
  Tail cs -> nearestCircleInList cs m old
  Tree _ _ first second -> nearestCircleBruteForce first m $ nearestCircleBruteForce second m old

