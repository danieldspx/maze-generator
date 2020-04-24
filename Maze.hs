module Maze (
    Cell,
    getCellList,
    getMazeSvg
) where

import Svg
import qualified Data.Map as Map

type Group = Int
data Cell = Cell Int Int Group deriving (Show, Eq) -- X Y

tupleToCell :: (Int, Int) -> Cell
tupleToCell (x, y) = Cell x y 0

cellToRect :: Int -> Cell -> Rect
cellToRect sizeCell (Cell x y _) = Rect {pos = Coord x y, dimen = Dimen sizeCell sizeCell}

getCellList :: [(Int, Int)] -> [Cell]
getCellList tuples = map tupleToCell tuples

getMazeSvg :: Int -> Int -> [Cell] -> String
getMazeSvg sizeSvg sizeCell cells = createSvgContent sizeSvg sizeCell $ map (cellToRect sizeCell) cells

getCellsCoordinate :: Int -> [Cell]
getCellsCoordinate size = [Cell x y 0 | x <- [0..size], y <- [0..size], x < size, y < size]

createGroupForCell :: Cell -> Int -> Cell
createGroupForCell (Cell x y _) group = Cell x y group 

createGroupForEachCell :: Int -> [Cell] -> [Cell]
createGroupForEachCell _ [] = []
createGroupForEachCell startGroup (c:[]) = [createGroupForCell c startGroup]
createGroupForEachCell startGroup (c:cells) = [createGroupForCell c startGroup]++createGroupForEachCell (startGroup+1) cells

getCellsWithGroup :: Int -> [Cell]
getCellsWithGroup size = createGroupForEachCell 1 $ getCellsCoordinate size

-- Graph
-- data Graph = EmptyGraph | Node {cellG::Cell, topG::Graph, rightG::Graph, bottomG::Graph, leftG::Graph} deriving (Show, Eq)
-- singletonGraph :: Cell -> Graph
-- singletonGraph cell = Node  {cellG = cell, topG=EmptyGraph, rightG=EmptyGraph, bottomG=EmptyGraph, leftG=EmptyGraph}
-- Map.
-- % Modular pra acessar
-- Usar Map