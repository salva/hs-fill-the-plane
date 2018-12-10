import Data.Complex
import Lucid.Svg
--import Data.List.Unique (sortUniq)
import System.Random
import Debug.Trace

import System.Environment

import Geo
import Tree
import Metrics
import Random
import Svg
import Apollonius

svgTreeLines tree box = svgLines $ treeLines tree box

drawRandomTree fn n = do
  -- gen0 <- getStdGen
  let gen0 = mkStdGen 1
  let (tree, gen1) = randomTree n gen0
  writeTree tree fn
  return (tree, gen1)

writeTree tree fn = do
  putStrLn $ "writing file " ++ fn
  writeFile fn $ show $ svg $ svgTreeWithLines $ tree

main = do
  [fn, n] <- getArgs
  drawRandomTree fn ((read n)::Int)

svgTreeWithLines tree = do
  svgCircles $ treeCircles tree
  svgLines $ treeLines tree (Box 0 (1 :+ 1))

svgTreeWithLinesPlus tree circles = do
  svgTreeWithLines tree
  svgCirclesColor circles "red"

writeTreePlus tree circles fn = do
  putStrLn $ "writing file " ++ fn
  writeFile fn $ show $ svg $ svgTreeWithLinesPlus tree circles

drawRandomTreePlus fn n = do
  let gen0 = mkStdGen 1
  let (tree, gen1) = randomTree n gen0
  let (circle, circles, gen2) = randomCirclePlus tree gen1
  let (ca : cb : _) = circles
  let (Just cc, _) = nearestCircle tree (DistToPointMetric (0.918 :+ 0.364)) (Nothing, inf)
  let sols = apollonius ca cb cc
  writeTreePlus tree (circle:ca:cb:cc:sols) fn
  return circles

