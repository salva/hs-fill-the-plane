import Data.Complex
import Data.List (partition, sort)
import Lucid.Svg
import Data.List.Unique (sortUniq)
import System.Random
import Debug.Trace
import Data.Text (pack)
import Data.Text.Lazy (unpack)
import qualified Data.PQueue.Prio.Min as Q

{-# LANGUAGE OverloadedStrings #-}

tr :: (Show b) => String -> b -> b
--tr s a = trace (s ++ ": " ++ (show a)) a
tr _ a = a

type R = Double

type V = Complex R

v::R -> R -> V
v a b = a :+ b

x::V -> R
x (a :+ _) = a
y::V -> R
y (_ :+ b) = b

data Circle = Circle { getCenter::V, getRadius::R }
  deriving (Show, Eq)

instance Ord Circle where
  compare (Circle (x0 :+ y0) r0) (Circle (x1 :+ y1) r1) =
    (compare r0 r1) `mappend` (compare x0 x1) `mappend` (compare y0 y1)

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

maxTailSize = 6

orientationF X = x
orientationF Y = y

--insertMany :: Tree -> [Circle] -> Tree
--insertMany = foldl insert

insertManyTrace tree0 [] = tree0
insertManyTrace tree0 (c:cs) =
  let tree1 = insert tree0 fullPlaneBox c
      txt = unpack $ prettyText $ svg 100 $ do
        svgTreeLines tree1 (Box 0 (100 :+ 100))
        svgTreeCircles tree1
        svgCircle c "yellow"
  in insertManyTrace (trace txt tree1) cs

-- insert :: Tree -> Circle -> Tree
-- insert tree circle@(Circle c r) =
--   case tree of
--     Tail circles -> newTree (circle:circles)
--     Tree orientation pivot first second ->
--       let f = orientationF orientation
--           newFirst  = if (((f c) - r) >= pivot)
--                       then first
--                       else insert first circle
--           newSecond = if (((f c) + r) <= pivot)
--                       then second
--                       else insert second circle
--       in Tree orientation pivot newFirst newSecond

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


data Border = Border { getL :: R, getEnd :: Int, getStart :: Int }
  deriving (Show, Eq, Ord)

circleBorders :: (V -> R) -> R -> R -> Circle -> [Border]
circleBorders f b0 b1 (Circle v r) =
  let l = f v
      l0 = max (l - r) b0
      l1 = min (l + r) b1
  in [Border l0 0 1, Border l1 1 0]


bestPivot1D :: (V -> R) -> R -> R -> [Circle] -> (Maybe R, Int)
bestPivot1D f b0 b1 circles =
  let borders = sort $ circles >>= (circleBorders f b0 b1)
      n = length circles
      entropy0 = 2 * n * n
      (_, _, _, _, bestL, bestEntropy) = foldl evaluate (0, n, b0, 2, Nothing, entropy0) borders
        where evaluate (args@(before, after, lastL, lastEnd, bestL, bestEntropy)) (border@(Border nextL nextEnd nextStart)) =
                let entropy = (sqr $ 2 * before - n) + (sqr $ 2 * after - n)
                in if (lastEnd /= nextEnd) && (entropy < bestEntropy)
                   then (before + nextStart, after - nextEnd, nextL, nextEnd, Just (0.5 * (lastL + nextL)), entropy)
                   else (before + nextStart, after - nextEnd, nextL, nextEnd, bestL, bestEntropy)
  in (bestL, bestEntropy)

bestPivot (Box p0 p1) circles =
  let (bestX, bestEntropyX) = bestPivot1D x (x p0) (x p1) circles
      (bestY, bestEntropyY) = bestPivot1D y (y p0) (y p1) circles
  in case bestX of
       Nothing -> case bestY of
                    Nothing -> Nothing
                    Just y  -> Just (Y, y)
       Just x  -> case bestY of
                    Nothing -> Just (X, x)
                    Just y  -> Just $ if bestEntropyX < bestEntropyY then (X, x) else (Y, y)

emptyTree = Tail []


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

dist2 c0 c1 = realPart (dc * (conjugate dc))
  where dc = c0 - c1

dist :: V -> V -> R
dist a b = sqrt $ dist2 a b

pointBoxDist2 :: V -> Box -> R
pointBoxDist2 (x :+ y) (Box (x0 :+ y0) (x1 :+ y1)) =
  let bestX = if x >= x1 then x1 else if x <= x0 then x0 else x
      bestY = if y >= y1 then y1 else if y <= y0 then y0 else y
      dx = bestX - x
      dy = bestY - y
  in dx * dx + dy * dy

pointBoxDist :: V -> Box -> R
pointBoxDist p box = sqrt $ pointBoxDist2 p box

circleTouchesBox :: Circle -> Box -> Bool
circleTouchesBox (Circle c r) box = (pointBoxDist2 c box) < r * r

map2 _ [] = []
map2 f (a1:a2:as) = (f a1 a2):(map2 f as)
map3 _ [] = []
map3 f (a1:a2:a3:as) = (f a1 a2 a3):(map3 f as)

boxArea (Box v0 v1) = (x d) * (y d)
  where d = v1 - v0

newCircle x0 y0 r = Circle (v x0 y0) r

circlesDist (Circle p0 r0) (Circle p1 r1) = max 0 ((dist p0 p1) - r0 - r1)

pointCircleDist p0 (Circle p1 r1) = max 0 ((dist p0 p1) - r1)

pointCircleDist2 p circle = sqr $ pointCircleDist p circle

divideBox :: Box -> Orientation -> R -> (Box, Box)
divideBox (Box (x0 :+ y0) (x1 :+ y1)) orientation pivot =
  case orientation of
    X -> (Box (x0 :+ y0) (pivot :+ y1), Box (pivot :+ y0) (x1 :+ y1))
    Y -> (Box (x0 :+ y0) (x1 :+ pivot), Box (x0 :+ pivot) (x1 :+ y1))

-- nearestCircle :: Tree -> V -> (Maybe Circle, R)
-- nearestCircle tree p = let box = fullPlaneBox
--                            queue = (Q.singleton inf (box, tree))::(Q.MinPQueue R (Box, Tree))
--                        in nearestInQueue queue p (Nothing, inf)
--   where nearestInQueue queue p old@(best,minD2) =
--           case (Q.getMin queue) of
--             Nothing -> (best, minD2)
--             Just (d2, (box, tree)) ->
--               if d2 > minD2
--               then old
--               else let queue' = Q.deleteMin queue
--                    in case tree of
--                         Tail circles -> nearestInQueue queue' p $ nearestInList circles p old
--                         Tree orientation pivot first second ->
--                           let (firstBox, secondBox) = divideBox box orientation pivot
--                               firstD2 = pointBoxDist2 p firstBox
--                               secondD2 = pointBoxDist2 p secondBox
--                               queue'' = Q.insert firstD2 (firstBox, first) queue'
--                               queue''' = Q.insert secondD2 (secondBox, second) queue''
--                           in nearestInQueue queue''' p old
--         nearestInList list p old = foldl pickBest old list
--         pickBest old@(_, minD2) circle = let d2 = pointCircleDist2 p circle
--                                          in if d2 < minD2
--                                             then (Just circle, d2)
--                                             else old

class Metric t where
  boxDist :: t -> Box -> R
  circleDist :: t -> Circle -> R

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


sqr :: (Num a) => a -> a
sqr a = a * a

newtype DistToPointMetric = DistToPointMetric { getV :: Complex Double }
  deriving Show

instance Metric DistToPointMetric where
  boxDist m box = tr ("boxDist "++(show m)++", "++(show box)) $ pointBoxDist2 (getV m) box
  circleDist m circle = tr ("circleDist "++(show m)++", "++(show circle)) $ pointCircleDist2 (getV m) circle

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

recoordinateCircle :: V -> V -> Circle -> Circle
recoordinateCircle o (xu :+ yu) (Circle c r) =
  let (xc :+ yc) = c - o
      idet = 1.0/(xu * xu + yu * yu)
      xc' = idet * (xc * xu + yc * yu)
      yc' = idet * (yc * xu - xc * yu)
      r' = r / sqrt idet
  in Circle (xc' :+ yc') r'

data AdjacentCircleRadiusMetric = AdjacentCircleRadiusMetric V V V

instance Metric AdjacentCircleRadiusMetric where
  boxDist (AdjacentCircleRadiusMetric c o v) b =
    tr ("A-boxDist "++(show (o,v,b))) $
    let (Box (x0 :+ y0) (x1 :+ y1)) = tr ("recoordinate "++(show (o, v, b))) $ recoordinateBox o v b
    in if x1 <= 0
       then tr ("x1 <= 0") inf
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
    tr ("A-circleDist "++(show (o,v,circle))) $
    if c == c1
    then inf
    else let (Circle (xc :+ yc) rc) = tr "reccordinateCircle" $ recoordinateCircle o v circle
             xc_rc = xc + rc
         in if xc_rc < 0
            then inf
            else 0.5 * (yc * yc / xc_rc + xc - rc)

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


scl s (x :+ y) = (s * x) :+ (s * y)

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
        cross (x0 :+ y0) (x1 :+ y1) = x0 * y1 - x1 * y0


-- randomCircle :: [R] -> (Circle, [R])
-- randomCircle (x : y : r : rs) = ((Circle (v x y) (0.2 * r)), rs)
-- randomCircles rs = let (circle, rs') = randomCircle rs
--                    in circle:(randomCircles rs')

data Line = Line V V

treeLines (Tail _) _ = []
treeLines (Tree orientation pivot first second) outerBox@(Box (x0 :+ y0) (x1 :+ y1)) =
  let line = case orientation of
               X -> (Line (pivot :+ y0) (pivot :+ y1))
               Y -> (Line (x0 :+ pivot) (x1 :+ pivot))
      (firstOuterBox, secondOuterBox) = divideBox outerBox orientation pivot
  in line : (treeLines first firstOuterBox) ++ (treeLines second secondOuterBox)

treeCircles (Tail circles) = circles
treeCircles (Tree _ _ f s) = treeCircles f ++ treeCircles s

-- foo a = do
--   gen <- getStdGen
--   let circles = take a $ randomCircles ((randoms gen)::[R])
--   let tree = newTree circles
--   putStrLn "Original"
--   mapM (putStrLn . show) circles
--   putStrLn "Tree"
--   putStrLn $ show tree
--   putStrLn "From tree"
--   mapM (putStrLn . show) $ sortUniq $ treeCircles tree
--   putStrLn "Non unique"
--   mapM (putStrLn . show) $ treeCircles $ tree
--   return ()

randomTree :: Int -> IO ()
randomTree n = do
  g <- getStdGen
  let circles = randomCircles g
  let strs = drawAdjustedCircles $ take n $ circles
  writeStrs 1 strs

writeStrs _ [] = return ()
writeStrs i (s:ss) = do
  let fn = "/tmp/circles/frame-"++(show i)++".svg"
  writeFile fn s
  putStrLn $ "writing " ++ fn
  writeStrs (i + 1) ss

svgCircle (Circle (x :+ y) r) color =
  circle_ [cx_ (pack $ show x), cy_ (pack $ show y), r_ (pack $ show r), stroke_ (pack color), fill_opacity_ $ pack "0"]
  -- circle_ [cx_ (pack $ show x), cy_ (pack $ show y), r_ (pack $ show r), color_ (pack color)]

svgCircles [] = return ()
svgCircles (c:cs) = do
  svgCircle c "green"
  svgCircles cs

scaleCircle s (Circle c r) = Circle (scl s c) (s * r)

svg size content = do
  doctype_
  with (svg11_ content) [version_ (pack "1.1"), width_ (pack $ show size) , height_ (pack $ show size)]

svg200 = svg 200

svgLine (Line (x1 :+ y1) (x2 :+ y2)) =
  line_ [x1_ (pack $ show x1), y1_ (pack $ show y1), x2_ (pack $ show x2), y2_ (pack $ show y2), stroke_ $ pack "black"]

svgLines [] = return ()
svgLines (l:ls) = do
  svgLine l
  svgLines ls

svgTreeCircles tree = svgCircles $ treeCircles tree

svgTreeLines tree box = svgLines $ treeLines tree box

printSvgTree tree size = print $ svg size $ do
  svgTreeLines tree (Box 0 (size :+ size))
  svgTreeCircles tree

-- drawRandomTree :: Int -> IO ()
drawRandomTree n = do
  gen0 <- getStdGen
  let (tree, gen1) = insertRandomCircles (newTree []) n gen0
  print $ svg200 $ svgTreeCircles tree
  return ()

insertRandomCircle :: (RandomGen g) => Tree -> g -> (Tree, g)
insertRandomCircle tree0 gen0 = let (circle, gen1) = randomCircle tree0 gen0
                                    tree1 = insert tree0 fullPlaneBox circle
                                in (tree1, gen1)

insertRandomCircles :: (RandomGen g) => Tree -> Int -> g -> (Tree, g)
insertRandomCircles tree0 n gen0 = let (tree, gen) =
                                         if n == 0
                                         then (tree0, gen0)
                                         else let (tree1, gen1) = insertRandomCircle tree0 gen0
                                              in insertRandomCircles tree1 (n - 1) gen1
                                   in (tr "tree" tree, gen)

-- svgTreeTouchingBoxes :: (Metric m) => Tree -> Box -> m -> R -> ?
svgTreeTouchingBoxes tree box m best =
  let boxes = treeTailBoxes tree box
      boxesWithMetric = map (\box -> tr "boxWithMetric" (box, boxDist m box)) boxes
      filtered = [box | (box, dist) <- boxesWithMetric, dist < best]
  in svgBoxes filtered

svgBoxes [] = return ()
svgBoxes ((Box (x0 :+ y0) (x1 :+ y1)):bs) =
  let dx = x1 - x0
      dy = y1 - y0
  in do rect_ [width_ $ pack $ show dx, height_ $ pack $ show dy, x_ $ pack $ show x0, y_ $ pack $ show y0, fill_ $ pack "red"]
        svgBoxes bs

  -- if (boxDist m box) >= best
  -- then return ()
  -- else case tree of
  --   Tail _ ->
  --     let (Box (x0 :+ y0) (x1 :+ y1)) = box
  --         dx = x1 - x0
  --         dy = y1 - y0
  --     in rect_ [width_ $ pack $ show dx, height_ $ pack $ show dy, x_ $ pack $ show x0, y_ $ pack $ show y0, fill_ $ pack "red"]
  --   Tree orientation pivot first second ->
  --     let (firstBox, secondBox) = divideBox box orientation pivot
  --     in do
  --       svgTreeTouchingBoxes first firstBox m best
  --       svgTreeTouchingBoxes second secondBox m best



treeTailBoxes :: Tree -> Box -> [Box]
treeTailBoxes tree box = case tree of
  Tail _ -> [box]
  Tree orientation pivot first second -> let (firstBox, secondBox) = divideBox box orientation pivot
                                         in (treeTailBoxes first firstBox) ++ (treeTailBoxes second secondBox)


svgMaybeCircle Nothing _ = return ()
svgMaybeCircle (Just circle) color = svgCircle circle color

adjustCircle :: Tree -> Circle -> (Circle, String)
adjustCircle tree (Circle p r) =
  let svgBox = Box 0 (200 :+ 200)
      (nearest1, _) = nearestCircle tree (DistToPointMetric p) (Nothing, r * r)
      (nearest2, r2, p2, svg2) = case nearest1 of
                                   Nothing -> (Nothing, r, p, return ())
                                   Just (Circle c1 r1) -> let v = if p == c1 then 1 else p - c1
                                                              v1 = scl (1.0 / (magnitude v)) v
                                                              o = c1 + (scl r1 v1)
                                                              -- FIXME: remove the BruteForce part
                                                              (nearest2, r2) = nearestCircleBruteForce tree (AdjacentCircleRadiusMetric c1 o v1) (Nothing, r)
                                                              p2 = o + (scl r2 v1)
                                                              svg2 = svgTreeTouchingBoxes tree svgBox (AdjacentCircleRadiusMetric c1 o v1) r
                                                          in (nearest2, r2, p2, svg2)
      out = Circle p2 r2
      txt = unpack $ prettyText $ svg 200 $ do
        svgTreeLines tree (Box 0 (200 :+ 200))
        svg2
        svgTreeCircles tree
        svgCircle (Circle p r) "gray"
        svgMaybeCircle nearest1 "yellow"
        svgMaybeCircle nearest2 "orange"
        svgCircle out "blue"
  in (out, txt)

drawAdjustedCircles :: [Circle] -> [String]
drawAdjustedCircles cs = drawAdjustedCirclesWithTree emptyTree cs

drawAdjustedCirclesWithTree :: Tree -> [Circle] -> [String]
drawAdjustedCirclesWithTree _ [] = []
drawAdjustedCirclesWithTree tree (c:cs) =
  let (c1, txt) = adjustCircle tree c
      tree1 = insert tree fullPlaneBox c1
  in (txt : (drawAdjustedCirclesWithTree tree1 cs))

randomCircles :: (RandomGen g) => g -> [Circle]
randomCircles gen0 =
  let (x, gen1) = randomR (0, 200) gen0
      (y, gen2) = randomR (0, 200) gen1
      (r, gen3) = randomR (10, 40) gen2
  in (Circle (x :+ y) r):(randomCircles gen3)

randomCircle :: (RandomGen g) => Tree -> g -> (Circle, g)
randomCircle tree gen0 =
  let (x, gen1) = randomR (0, 200) gen0
      (y, gen2) = randomR (0, 200) gen1
      (r, gen3) = randomR (10, 40) gen2
      p = v x y
      (nearest1, _) = tr ("nearestCircle 1 (p:" ++ (show p) ++", r:"++(show r)++")") $ nearestCircle tree (DistToPointMetric p) (Nothing, r * r)
      newCircle = tr "newCircle" $ case nearest1 of
        Nothing -> Circle p r
        Just (Circle c1 r1) -> let v = if p == c1 then 1 else p - c1
                                   v1 = scl (1.0 / (magnitude v)) v
                                   o = c1 + (scl r1 v1)
                                   (nearest2, d2) = nearestCircleBruteForce tree (AdjacentCircleRadiusMetric c1 o v1) (Nothing, r)
                               in case nearest2 of
                                    Nothing -> Circle (o + (scl r v1)) r
                                    Just (Circle c2 r2) -> Circle (o + (scl d2 v1)) d2
      txt = unpack $ prettyText $ svg200 $ do
        svgTreeLines tree (Box 0 (200 :+ 200))
        case nearest1 of
          Just (Circle c1 r1) -> let v = if p == c1 then 1 else p - c1
                                     v1 = scl (1.0 / (magnitude v)) v
                                     o = c1 + (scl r1 v1)
                                 in svgTreeTouchingBoxes tree (Box 0 (200 :+ 200)) (AdjacentCircleRadiusMetric c1 o v1) r
          Nothing -> return ()
        svgTreeCircles tree
        case nearest1 of
          Just circle -> svgCircle circle "pink"
          Nothing -> return ()
        svgCircle (Circle (x :+ y) r) (if nearest1 == Nothing then "yellow" else "blue")

  in (trace txt newCircle, gen3)


sampleTree = insertManyTrace (newTree []) [Circle (0 :+ 10) 5,
                                           Circle (10 :+ 60) 15,
                                           Circle (20 :+ 20) 5,
                                           Circle (30 :+ 30) 2.5,
                                           Circle (40 :+ 40) 10,
                                           Circle (50 :+ 0) 15,
                                           Circle (60 :+ 70) 5,
                                           Circle (70 :+ 50) 25 ]
  
