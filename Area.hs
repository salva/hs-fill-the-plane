-- Areas

boxCircleIntersectionArea :: Box -> Circle -> R
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

-- TODO: make into a "where" clause of the function above
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

