module Apollonius where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (fromList, (!))
import Data.Complex
import Debug.Trace
import Geo


--import Data.Vector.Unboxed


apollonius :: Circle -> Circle -> Circle -> [Circle]

-- EQ₀: (x - x₀)² + (y - y₀)² - (r + r₀)² = 0
-- EQ₁: (x - x₁)² + (y - y₁)² - (r + r₁)² = 0
-- EQ₂: (x - x₂)² + (y - y₂)² - (r + r₂)² = 0

-- expanded:
-- x₀² + y₀² - r₀² - 2x₀x - 2y₀y - 2r₀r + x² + y² + r²= 0
-- x₁² + y₁² - r₁² - 2x₁x - 2y₁y - 2r₁r + x² + y² + r²= 0
-- x₂² + y₂² - r₂² - 2x₂x - 2y₂y - 2r₂r + x² + y² + r²= 0

-- EQ₀₁ = EQ₀ - EQ₁: x₀² - x₁² + y₀² - y₁² - r₀² + r₁² - 2(x₀ - x₁)x - 2(y₀ - y₁)y - 2(r₀ - r₁)r = 0
-- EQ₀₂ = EQ₀ - EQ₂: x₀² - x₂² + y₀² - y₂² - r₀² + r₂² - 2(x₀ - x₂)x - 2(y₀ - y₂)y - 2(r₀ - r₂)r = 0

-- reordered:
-- EQ₀₁: 2(x₀ - x₁)x + 2(y₀ - y₁)y = x₀² - x₁² + y₀² - y₁² - r₀² + r₁² - 2(r₀ - r₁)r = b₀₁ - 2r₀₁r
-- EQ₀₂: 2(x₀ - x₂)x + 2(y₀ - y₂)y = x₀² - x₂² + y₀² - y₂² - r₀² + r₂² - 2(r₀ - r₂)r = b₀₂ - 2r₀₂r

-- det₁ = 4((x₀ - x₁)(y₀ - y₂) - (y₀ - y₁)(x₀ - x₂))


circleEq (Circle (xc :+ yc) rc) = fromList [xc*xc+yc*yc-rc*rc, -2*xc, -2*yc, -2*rc, 1, 1, -1]

det eqA eqB ixA ixB =
  (eqA!ixA)*(eqB!ixB) - (eqA!ixB)*(eqB!ixA)

bestDet eqA eqB permutations = foldl pickBest (0, (0,0,0)) $ map detAndPerm permutations
  where detAndPerm perm@(a, b, _) = (det eqA eqB a b, perm)
        pickBest bestSoFar@(bestDet, bestPerm) current@(det, perm) =
          if (abs bestDet) >= (abs det) then bestSoFar else current

comb s0 s1 v0 v1 = s0 * v0 + s1 * v1

sqrEq eq c c2 =
  let a0 = eq!0
      a2 = eq!c2
      a0_2 = -a0 * a0
      a2_2 = -a2 * a2
      a0a2 = -2 * a0 * a2
  in fromList $ case c of
    1 -> case c2 of 
      2 -> [a0_2,    0, a0a2,    0,    1, a2_2,    0] -- x y
      3 -> [a0_2,    0,    0, a0a2,    1,    0, a2_2] -- x r
    2 -> case c2 of
      1 -> [a0_2, a0a2,    0,    0, a2_2,    1,    0] -- y x
      3 -> [a0_2,    0,    0, a0a2,    0,    1, a2_2] -- y r
    3 -> case c2 of
      1 -> [a0_2, a0a2,    0,    0, a2_2,    0,    1] -- r x
      2 -> [a0_2,    0, a0a2,    0,    0, a2_2,    1] -- r y

solveEq2 a b c =
  if a == 0
  then [-c/b]
  else let d = b*b-4*a*c
       in if d < 0
          then []
          else if d == 0
               then [-b/(2*a)]
               else let sqrtD = sqrt d
                    in [(b + sqrtD)/(-2*a), (b - sqrtD)/(-2*a)]

apollonius circleA circleB circleC =
  let eqA = circleEq circleA
      eqB = circleEq circleB
      eqC = circleEq circleC
      eqAB = VU.zipWith (-) eqA eqB
      eqAC = VU.zipWith (-) eqA eqC
      (det, (c0, c1, c2)) = bestDet eqAB eqAC [(1,2,3), (2,3,1), (3,1,2)]
  in if det == 0
     then []
     else let idet = 1/det
              eqAB1 = VU.zipWith (comb (idet*(eqAC!c1)) (-idet*(eqAB!c1))) eqAB eqAC
              eqAC1 = VU.zipWith (comb (idet*(eqAB!c0)) (-idet*(eqAC!c0))) eqAC eqAB
              eqAB2 = sqrEq eqAB1 c0 c2
              eqAC2 = sqrEq eqAC1 c1 c2
              c0_2 = c0 + 3
              c1_2 = c1 + 3
              c2_2 = c2 + 3
              eqA1 = VU.zipWith (comb 1 (-(eqA !c0_2))) eqA eqAB2
              eqA2 = VU.zipWith (comb 1 (-(eqA1!c1_2))) eqA1 eqAC2
              eqA3 = VU.zipWith (comb 1 (-(eqA2!c0))) eqA2 eqAB1
              eqA4 = VU.zipWith (comb 1 (-(eqA3!c1))) eqA3 eqAC1
              a = eqA4!c2_2
              b = eqA4!c2
              c = eqA4!0
              v2s = solveEq2 a b c
              v0s = map (\v2 -> -(eqAB1!0 + v2*eqAB1!c2)) v2s
              v1s = map (\v2 -> -(eqAC1!0 + v2*eqAC1!c2)) v2s
          in filter positiveRadius $ case c0 of
               1 -> zipWith3 makeCircle v0s v1s v2s
               2 -> zipWith3 makeCircle v2s v0s v1s
               3 -> zipWith3 makeCircle v1s v2s v0s
  where makeCircle a b c = Circle (a :+ b) c
        positiveRadius (Circle _ r) = r >= 0
