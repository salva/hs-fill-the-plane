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

