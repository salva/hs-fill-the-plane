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
  in return () --svgBoxes filtered

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
  

