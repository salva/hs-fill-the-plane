module ColorfulTree where

import System.Random (RandomGen, randomR)
import Data.Maybe (catMaybes)
import Data.List ((\\), (!!))
import qualified Data.Map as Map

import Geo
import Tree
import Random (randomCircleWithNeighbors)

type Color = String

data ColorfulTree = ColorfulTree Tree (Map.Map Circle Color)

insertCircle :: ColorfulTree -> Box -> Circle -> Color -> ColorfulTree
insertCircle (ColorfulTree tree0 colorMap0) box circle color =
  let colorMap1 = Map.insert circle color colorMap0
      tree1 = insert tree0 box circle
  in ColorfulTree tree1 colorMap1

randomChoice :: (RandomGen g) => [a] -> g -> (a, g)
randomChoice as g0 =
  let last = length as - 1
      (choice, g1) = randomR (0, last) g0
  in (as!!choice, g1)

insertRandomCircle :: (RandomGen g) => ColorfulTree -> Box -> [Color] -> g -> (ColorfulTree, g)
insertRandomCircle colorfulTree@(ColorfulTree tree colorMap) box colors g0 =
  let (circle, neighbors, g1) = randomCircleWithNeighbors tree box g0
      usedColors = catMaybes $ map (\k -> Map.lookup k colorMap) neighbors
      freeColors = colors \\ usedColors
      (color, g2) = randomChoice freeColors g1
  in (insertCircle colorfulTree box circle color, g2)

insertRandomCircles :: (RandomGen g) => Int -> ColorfulTree -> Box -> [Color] -> g -> (ColorfulTree, g)
insertRandomCircles n colorfulTree0 box colors g0
  | n == 0    = (colorfulTree0, g0)
  | otherwise = let (colorfulTree1, g1) = insertRandomCircle colorfulTree0 box colors g0
                in insertRandomCircles (n - 1) colorfulTree1 box colors g1

emptyColorfulTree :: ColorfulTree
emptyColorfulTree = ColorfulTree emptyTree Map.empty

randomColorfulTree :: (RandomGen g) => Int -> Box -> [Color] -> g -> (ColorfulTree, g)
randomColorfulTree n = insertRandomCircles n emptyColorfulTree

colorfulTreeCirclesAndColors :: ColorfulTree -> [(Circle, Color)]
colorfulTreeCirclesAndColors (ColorfulTree _ colorMap) = Map.assocs colorMap
