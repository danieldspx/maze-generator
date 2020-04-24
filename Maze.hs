module Maze (
    Cell,
    getCellList,
    getMazeSvg
) where

import Svg
import qualified Data.Map as Map

type Group = Int
data Cell = AbsentCell | Cell Int Int deriving (Show, Eq, Ord) -- X Y
data CellProp = CellProp {group::Group, top::Cell, right::Cell, bottom::Cell, left::Cell} deriving (Show)
-- A CellProp with `group = 0` means that it has no group. A group must be >= 1.

instance Eq CellProp where
    cellP1 == cellP2 = (group cellP1) == (group cellP2) -- CellProp are equal when their group are.

tupleToCell :: (Int, Int) -> Cell
tupleToCell (x, y) = Cell x y

cellToRect :: Int -> Cell -> Rect
cellToRect sizeCell (Cell x y) = Rect {pos = Coord x y, dimen = Dimen sizeCell sizeCell}

getCellList :: [(Int, Int)] -> [Cell]
getCellList tuples = map tupleToCell tuples

getMazeSvg :: Int -> Int -> [Cell] -> String
getMazeSvg sizeSvg sizeCell cells = createSvgContent sizeSvg sizeCell $ map (cellToRect sizeCell) cells

getCellsCoordinate :: Int -> [Cell]
getCellsCoordinate size = [Cell x y | x <- [0..size], y <- [0..size], x < size, y < size]

getTopNeighbour :: Cell -> Cell
getTopNeighbour AbsentCell = AbsentCell 
getTopNeighbour (Cell x y) = if y == 0 then AbsentCell else Cell x (pred y)

getRightNeighbour :: Int -> Cell -> Cell
getRightNeighbour _ AbsentCell = AbsentCell 
getRightNeighbour size (Cell x y) = if x == (size-1) then AbsentCell else Cell (succ x) y

getBottomNeighbour :: Int -> Cell -> Cell
getBottomNeighbour _ AbsentCell = AbsentCell 
getBottomNeighbour size (Cell x y) = if y == (size-1) then AbsentCell else Cell x (succ y)

getLeftNeighbour :: Cell -> Cell
getLeftNeighbour AbsentCell = AbsentCell 
getLeftNeighbour (Cell x y) = if x == 0 then AbsentCell else Cell (pred x) y

getEmptyCellProp :: CellProp
getEmptyCellProp = CellProp {group=0,top=AbsentCell,right=AbsentCell,bottom=AbsentCell,left=AbsentCell}

getNeighbours :: Int -> Cell -> CellProp
getNeighbours _ AbsentCell = CellProp {group=0,top=AbsentCell,right=AbsentCell,bottom=AbsentCell,left=AbsentCell}
getNeighbours size cell = CellProp {group=0,top=(getTopNeighbour cell),
                                    right=(getRightNeighbour size cell),
                                    bottom=(getBottomNeighbour size cell),
                                    left=(getLeftNeighbour cell)}


setGroupCellProp :: Int -> CellProp -> CellProp
setGroupCellProp group cellProp = CellProp {group=group,
                                                top=(top cellProp),
                                                right=(right cellProp),
                                                bottom=(bottom cellProp),
                                                left=(left cellProp)} 

_setGroupTupleCells :: Int -> [(Cell, CellProp)] -> [(Cell, CellProp)]
_setGroupTupleCells _ [] = [] 
_setGroupTupleCells startGroup ((cell, cellProp):[]) = [(cell, setGroupCellProp startGroup cellProp)]
_setGroupTupleCells startGroup (x:xs) = (_setGroupTupleCells startGroup [x])++(_setGroupTupleCells (startGroup+1) xs)

setGroupMapCells :: Int -> Map.Map Cell CellProp -> Map.Map Cell CellProp
setGroupMapCells startGroup cells = Map.fromList $ _setGroupTupleCells startGroup $ Map.toList cells

getMapCells :: Int -> Map.Map Cell CellProp
getMapCells size = Map.fromList tuplesCellNeighbour
                                    where
                                        cellsCord = getCellsCoordinate size
                                        neighbours = map (getNeighbours size) $ cellsCord
                                        tuplesCellNeighbour = zip cellsCord neighbours

getMapCellsWithGroup :: Int -> Int -> Map.Map Cell CellProp
getMapCellsWithGroup size startGroup = setGroupMapCells startGroup $ getMapCells size


-- createMapCells :: Int -> [(Cell, CellProp)] -> Map.Map Cell CellProp
-- createMapCells _ [] = Map.empty
-- createMapCells size (x:[]) = Map.
-- createMapCells size (x:xs) = Map.

-- createGroupForCell :: Cell -> Int -> Cell
-- createGroupForCell (Cell x y) group = Cell x y group 

-- createGroupForEachCell :: Int -> [Cell] -> [Cell]
-- createGroupForEachCell _ [] = []
-- createGroupForEachCell startGroup (c:[]) = [createGroupForCell c startGroup]
-- createGroupForEachCell startGroup (c:cells) = [createGroupForCell c startGroup]++createGroupForEachCell (startGroup+1) cells

-- getCellsWithGroup :: Int -> [Cell]
-- getCellsWithGroup size = createGroupForEachCell 1 $ getCellsCoordinate size

-- map

-- Graph
-- data Graph = EmptyGraph | Node {cellG::Cell, topG::Graph, rightG::Graph, bottomG::Graph, leftG::Graph} deriving (Show, Eq)
-- singletonGraph :: Cell -> Graph
-- singletonGraph cell = Node  {cellG = cell, topG=EmptyGraph, rightG=EmptyGraph, bottomG=EmptyGraph, leftG=EmptyGraph}
-- Map.
-- % Modular pra acessar
-- Usar Map