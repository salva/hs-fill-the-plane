module Svg where

import Data.Complex
import Data.Text (pack)
import Data.Text.Lazy (unpack)
import Lucid.Svg
import Lucid.Base (HtmlT)
import Data.String (fromString)

import Geo


--{-# LANGUAGE OverloadedStrings #-}

roundComplex (x :+ y) = (show $ r x) ++ "i" ++ (show $ r y)
  where r x = floor (1000 * x)

size = 1024
ps a = fromString $ show a

svgCircle :: Monad m => Circle -> String -> Lucid.Base.HtmlT m ()
svgCircle (Circle p@(x :+ y) r) color =
   do
     let fontSize = ps (1024 * r / 10)
     circle_ [cx_ $ ps $ size * x, cy_ $ ps $ size * y, r_ $ ps $ size * r, stroke_ $ fromString color, fill_opacity_ $ fromString "0"]
     text_ [x_ $ ps $ size * x, y_ $ ps $ size * y, font_size_ fontSize] $ fromString $ roundComplex p

svgCircles cs = svgCirclesColor cs "green"

svgCirclesColor circles color = mapM (\circle -> svgCircle circle (fromString color)) circles

svgLine (Line (x1 :+ y1) (x2 :+ y2)) =
  line_ [x1_ $ ps $ size * x1, y1_ $ ps $ size * y1, x2_ $ ps $ size * x2, y2_ $ ps $ size * y2, stroke_ $ fromString "black"]

svgLines [] = return ()
svgLines (l:ls) = do
  svgLine l
  svgLines ls

svg content = do
  doctype_
  with (svg11_ content) [version_ $ fromString "1.1", width_ $ ps size, height_ $ ps size]

svgBoxes [] = return ()
svgBoxes ((Box (x0 :+ y0) (x1 :+ y1)):bs) =
  let dx = x1 - x0
      dy = y1 - y0
  in do rect_ [ width_ $ ps $ size * dx, height_ $ ps $ size * dy,
                x_ $ ps $ size * x0, y_ $ ps $ size * y0,
                fill_ $ fromString "red" ]
        svgBoxes bs


