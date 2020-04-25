module Maze (
    Cell,
    CellProp,
    Group,
    initializeMazeAndGenerate,
    getMazeSvg
) where

import Svg
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace as Deb

type Group = Int
data Cell = AbsentCell | Cell Int Int | CantConnect deriving (Show, Eq, Ord) -- X Y
data CellProp = AbsentProp | CellProp {group::Group, top::Cell, right::Cell, bottom::Cell, left::Cell} deriving (Show, Ord)
-- A CellProp with `group = 0` means that it has no group. A group must be >= 1.
-- Neighbour of a Cell means connection (Wrong name but I might refactor it later, or not)

instance Eq CellProp where
    cellP1 == cellP2 = (group cellP1) == (group cellP2) -- CellProp are equal when their group are.

divMod' :: Int -> Int -> Int
divMod' x = snd . divMod x

deleteNth :: Int -> [a] -> [a]
deleteNth i items = take i items ++ drop (1 + i) items

lookupProp :: Cell -> Map.Map Cell CellProp -> CellProp
-- lookupProp x y = Map.findWithDefault AbsentProp x y
lookupProp x y = Maybe.fromMaybe getEmptyCellProp $ Map.lookup x y

removeDuplicates :: (Foldable t, Eq a, Num a) => t a -> [a]
removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

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
getNeighbours _ AbsentCell = getEmptyCellProp
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
                                        -- neighbours = map (getNeighbours size) $ cellsCord
                                        tuplesCellNeighbour = zip cellsCord $ take (size*size) $ repeat getEmptyCellProp 

getMapCellsWithGroup :: Int -> Int -> Map.Map Cell CellProp
getMapCellsWithGroup size startGroup = setGroupMapCells startGroup $ getMapCells size

getConnection :: Int -> CellProp -> Cell
getConnection pos cellProp = case (pos `divMod'` 4) of 0 -> top cellProp
                                                       1 -> right cellProp
                                                       2 -> bottom cellProp
                                                       3 -> left cellProp
getNeighbourFromPos :: Cell -> Int -> Int -> Cell
getNeighbourFromPos cell size pos = case (pos `divMod'` 4) of 0 -> getTopNeighbour cell
                                                              1 -> getRightNeighbour size cell
                                                              2 -> getBottomNeighbour size cell
                                                              3 -> getLeftNeighbour cell

getPossibleConnDirection :: Int -> Cell -> [Int]
getPossibleConnDirection size cell = map fst $ filter (\x -> (snd x) /= AbsentCell) $ [(0, getTopNeighbour cell),
                                                                                        (1, getRightNeighbour size cell),
                                                                                        (2, getBottomNeighbour size cell),
                                                                                        (3, getLeftNeighbour cell)]

getExistingConnections :: CellProp -> [Cell]
getExistingConnections cellProp = filter (\c -> c/=AbsentCell && c/=CantConnect) $ [top cellProp,
                                                            right cellProp,
                                                            bottom cellProp,
                                                            left cellProp]

getAbsentConnection :: CellProp -> [Int] -> [(Int, Cell)]
getAbsentConnection cellProp possibleConn = filter (\x -> (snd x) == AbsentCell) $ map (\x -> (x, getConnection x cellProp)) possibleConn

shuffleList :: [Int] -> [a] -> [a]
shuffleList _ (x:[]) = [x]
shuffleList (n:[]) list = (list!!randPos):(deleteNth randPos list) where randPos = n `divMod'` (length list)
shuffleList (n:nums) list = (list!!randPos):(shuffleList nums listDel) where listDel = deleteNth randPos list
                                                                             randPos = n `divMod'` (length list)

canConnect :: CellProp -> CellProp -> Bool
canConnect AbsentProp _ = False 
canConnect _ AbsentProp = False
canConnect cellProp1 cellProp2 = cellProp1 /= cellProp2 -- If the groups are differents

searchPossibleConnection :: Int -> Map.Map Cell CellProp -> Cell -> [(Int, Cell)]
searchPossibleConnection size cellsMap cell = map (\(x, _) -> (x, getNeighbourFromPos cell size x)) $ filter (\x -> canConnect cellProp $ lookupProp (snd x ) cellsMap) absentNbr where absentNbr = getAbsentConnection cellProp possibleConn
                                                                                                                                                                                        possibleConn = getPossibleConnDirection size cell
                                                                                                                                                                                        cellProp = lookupProp cell cellsMap

stablishConnection :: Cell -> (Int, Cell) -> Map.Map Cell CellProp -> Map.Map Cell CellProp
stablishConnection CantConnect _ cellsMap = cellsMap
stablishConnection cell (connDirect, targetCell) cellsMap = let cellProp = lookupProp cell cellsMap
                                                                hasReverseConn fToGetCell = (fToGetCell targetProp) == cell
                                                                targetProp = lookupProp targetCell cellsMap
                                                                curriedMapInsert v = Map.insert cell v cellsMap 
                                                                in case connDirect of 0 -> Deb.trace (show cell++" -> "++show targetCell) $ stablishConnection targetCell (2, CantConnect) $ curriedMapInsert (cellProp {top = targetCell})
                                                                                      1 -> Deb.trace (show cell++" -> "++show targetCell) $ stablishConnection targetCell (3, CantConnect) $ curriedMapInsert (cellProp {right = targetCell})
                                                                                      2 -> Deb.trace (show cell++" -> "++show targetCell) $ stablishConnection targetCell (0, CantConnect) $ curriedMapInsert (cellProp {bottom = targetCell})
                                                                                      3 -> Deb.trace (show cell++" -> "++show targetCell) $ stablishConnection targetCell (1, CantConnect) $ curriedMapInsert (cellProp {left = targetCell})

initializeMazeAndGenerate :: Int -> [Int] -> Map.Map Cell CellProp
initializeMazeAndGenerate size randomList = generateMaze size randomCoords $ getMapCellsWithGroup size 1 where randomCoords = shuffleList randomList $ getCellsCoordinate size

generateMaze :: Int -> [Cell] -> Map.Map Cell CellProp -> Map.Map Cell CellProp
generateMaze _ [] cellsMap = cellsMap
generateMaze  size (thisCell:cells) cellsMap = generateMaze size cells updatedCellsMap where updatedCellsMap = if (length possibleConn) /= 0 then
                                                                                                            Deb.trace ("should CG "++show thisCell++" \n cellProp "++show cellProp) $ changeGroupAndPropagate thisCell (stablishConnection thisCell toConnectCell cellsMap) (group cellProp)
                                                                                                        else cellsMap
                                                                                             toConnectCell = (possibleConn!!0)
                                                                                             possibleConn = searchPossibleConnection size cellsMap thisCell
                                                                                             cellProp = lookupProp thisCell cellsMap

filterOnlyDifferentGroups :: Int ->  Map.Map Cell CellProp -> [Cell] -> [Cell]
filterOnlyDifferentGroups groupNum cellsMap cells = filter (\c -> groupNum /= (group (lookupProp c cellsMap))) cells 

changeGroupAndPropagate :: Cell -> Map.Map Cell CellProp -> Int -> Map.Map Cell CellProp
changeGroupAndPropagate cell cellsMap group = if length connections /= 0 then Deb.trace ("Propagate for "++show cell) $ propagateChildrenGroupChange connections cellsMap group else Deb.trace ("No connections"++show (getExistingConnections newCellProp)) $ updatedCellsMap
                                                where connections = filterOnlyDifferentGroups group updatedCellsMap $ getExistingConnections newCellProp 
                                                      updatedCellsMap = Map.insert cell newCellProp cellsMap
                                                      newCellProp = Deb.trace ("Setting GP "++show group++" TO "++show oldCellProp) $ setGroupCellProp group oldCellProp 
                                                      oldCellProp = lookupProp cell cellsMap


propagateChildrenGroupChange :: [Cell] -> Map.Map Cell CellProp -> Int -> Map.Map Cell CellProp
propagateChildrenGroupChange (c:[]) cellsMap group = (changeGroupAndPropagate c cellsMap group)
propagateChildrenGroupChange (c:cells) cellsMap group = Map.fromList $ (Map.toList $ changeGroupAndPropagate c cellsMap group)++(Map.toList $ propagateChildrenGroupChange cells cellsMap group)



                                                


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
-- DUPLA DEPENDENCIA, QUANDO `A` LIGA EM `B`, `B` TBM SE LIGA EM `A` 