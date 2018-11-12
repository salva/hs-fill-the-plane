
import Data.Complex
import Data.List (partition)

type R = Double

type V = Complex R

v::R -> R -> V
v a b = a :+ b

x::V -> R
x (a :+ _) = a
y::V -> R
y (_ :+ b) = b

data Divider = X R | Y R
  deriving Show

data Tree = Tree Divider Tree Tree | Tail [V]
  deriving Show

before::Divider -> V -> Bool
before (X a) v = if a < (x v)
  then True
  else False

before(Y a) v = if a < (y v)
  then True
  else False

maxTailSize = 3

insert::Tree -> V -> Tree
insert tree v = case tree of
  Tail a -> newTree (v:a)
  Tree div first second ->
    case (before div v) of
      True -> Tree div (insert first v) second
      False -> Tree div first (insert second v)

newTree vs = if (length vs) > maxTailSize
  then divideTree vs
  else Tail vs

divideTree vs = let (x0, x1) = interval $ map x vs
                    (y0, y1) = interval $ map y vs
                    dx = x1 - x0
                    dy = y1 - y0
                    div = if (dx > dy)
                      then X (0.5 * (x0 + x1))
                      else Y (0.5 * (y0 + y1))
                    (first, second) = partition (before div) vs
                in Tree div (newTail first) (newTail second)

interval::[R] -> (R, R)
interval [] = error "Can't calculate segment box for an empty list"
interval (v:vs) = foldl intervalAppend (v, v) vs
  where intervalAppend accu pivot =
          let (min, max) = accu
              newAccu | pivot < min = (pivot, max)
                      | pivot > max = (min, pivot)
                      | otherwise   = accu
          in newAccu
