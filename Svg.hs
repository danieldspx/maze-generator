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
svgBegin :: Int -> String
svgBegin size = printf "<svg width='%d' height='%d' xmlns='http://www.w3.org/2000/svg'>\n" size size

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

createRectXML :: Int -> Rect -> String
createRectXML sizeCell (Rect (Coord x y) (Dimen w h)) = 
    printf "<rect x='%d' y='%d' width='%d' height='%d' fill='red'/>\n" xReal yReal w h 
        where
            xReal = x*sizeCell
            yReal = y*sizeCell

createSvgContent :: Int -> Int -> [Rect] -> String
createSvgContent sizeSvg sizeCell  walls = (svgBegin $ sizeSvg*sizeCell)++(concat(map (createRectXML sizeCell) walls))++svgEnd
-- (svgBegin $ sizeSvg*sizeCell)++(concat(map (createRectXML sizeCell) walls))++[svgEnd]
