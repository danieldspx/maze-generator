module Maze (
    Wall,
    getWallList,
    getMazeSvg
) where

import Svg


data Wall = Wall Int Int -- X Y

tupleToWall :: (Int, Int) -> Wall
tupleToWall (x, y) = Wall x y

wallToRect :: Int -> Wall -> Rect
wallToRect sizeCell (Wall x y) = Rect {pos = Coord x y, dimen = Dimen sizeCell sizeCell}

getWallList :: [(Int, Int)] -> [Wall]
getWallList tuples = map tupleToWall tuples

getMazeSvg :: Int -> Int -> [Wall] -> String
getMazeSvg sizeSvg sizeCell walls = createSvgContent sizeSvg sizeCell $ map (wallToRect sizeCell) walls



