module Svg (
    Coord(..),
    Dimen(..),
    Rect(..),
    createSvgContent
) where

import Text.Printf

data Coord = Coord Int Int deriving (Show)
data Dimen = Dimen Int Int deriving (Show)
data Rect = Rect {pos :: Coord, dimen :: Dimen, fill :: String} deriving (Show)

-- String inicial do SVG
svgBegin :: Int -> Int -> String
svgBegin width height = 
    printf "<svg width='%d' height='%d' xmlns='http://www.w3.org/2000/svg'><rect width='100%%' height='100%%' fill='white'/>\n" width height

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

createRectXML :: Rect -> String
createRectXML (Rect (Coord x y) (Dimen w h) fill) = printf "<rect x='%d' y='%d' width='%d' height='%d' fill='%s'/>\n" x y w h fill

createSvgContent :: Int -> Int -> [Rect] -> String
createSvgContent wSvg hSvg  walls = (svgBegin wSvg hSvg)++(concat(map (createRectXML) walls))++svgEnd
