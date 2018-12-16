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
import ColorfulTree

svgTreeLines tree box = svgLines $ treeLines tree box

drawRandomTree fn n = do
  -- gen0 <- getStdGen
  let gen0 = mkStdGen 1
  let (tree, gen1) = randomTree n (Box 0 (1 :+ 1)) gen0
  writeTree tree fn
  return (tree, gen1)

writeTree tree fn = do
  putStrLn $ "writing file " ++ fn
  writeFile fn $ show $ svg $ svgTreeWithLines $ tree

svgTreeWithLines tree = do
  svgCircles $ treeCircles tree
  svgLines $ treeLines tree (Box 0 (1 :+ 1))


drawRandomColorfulTree fn n colors = do
  let g0 = mkStdGen 1
  let (colorfulTree, g1) = randomColorfulTree n (Box 0 (1 :+ 1)) colors g0
  writeColorfulTree colorfulTree fn
  return ()

writeColorfulTree colorfulTree fn = do
  putStrLn $ "Writing colorfulTree to file " ++ fn
  writeFile fn $ show $ svg $ svgColorfulTree colorfulTree

svgColorfulTree tree = svgCirclesWithColor $ colorfulTreeCirclesAndColors tree

main = do
  (fn:n:colors) <- getArgs
  drawRandomColorfulTree fn ((read n)::Int) colors

