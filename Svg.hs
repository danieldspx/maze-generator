module Svg (
    Coord(..),
    Dimen(..),
    Rect(..),
    createSvgContent
) where

import Text.Printf

data Coord = Coord Int Int -- X Y
data Dimen = Dimen Int Int -- Width Height
data Rect = Rect {pos :: Coord, dimen :: Dimen}

-- String inicial do SVG
svgBegin :: Int -> Int -> String
svgBegin width height = printf "<svg width='%d' height='%d' xmlns='http://www.w3.org/2000/svg'>\n" width height

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

createRectXML :: Rect -> String
createRectXML (Rect (Coord x y) (Dimen w h)) = printf "<rect x='%d' y='%d' width='%d' height='%d' fill='black'/>\n" x y w h

createSvgContent :: Int -> Int -> [Rect] -> String
createSvgContent wSvg hSvg  walls = (svgBegin wSvg hSvg)++(concat(map (createRectXML) walls))++svgEnd
