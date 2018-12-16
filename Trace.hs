module Trace where

import Debug.Trace (trace)

tR :: Show t => String -> t -> t
-- tR a b = trace (a ++ ": " ++ (show b)) b
tR _ b = b
