module Maze (
    generateMazeAndCreateSvg
) where

import Svg
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace as Deb

type Group = Int
data Cell = AbsentCell | Cell Int Int | CantConnect deriving (Show, Eq, Ord) -- X Y
data CellProp = AbsentProp | CellProp {group::Group, top::Cell, right::Cell, bottom::Cell, left::Cell} deriving (Show, Ord)
-- A CellProp with `group = 0` means that it has no group. A group must be >= 1.

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
    where cellsCord = getCellsCoordinate size
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
searchPossibleConnection size cellsMap cell = filter (\x -> canConnect cellProp $ lookupProp (snd x ) cellsMap) $ map (\(x, _) -> (x, getNeighbourFromPos cell size x)) absentNbr 
    where absentNbr = getAbsentConnection cellProp possibleConn
          possibleConn = getPossibleConnDirection size cell
          cellProp = lookupProp cell cellsMap

stablishConnection :: Cell -> (Int, Cell) -> Map.Map Cell CellProp -> Map.Map Cell CellProp
stablishConnection CantConnect _ cellsMap = cellsMap
stablishConnection cell (connDirect, targetCell) cellsMap = let cellProp = lookupProp cell cellsMap
                                                                hasReverseConn fToGetCell = (fToGetCell targetProp) == cell
                                                                targetProp = lookupProp targetCell cellsMap
                                                                curriedMapInsert v = Map.insert cell v cellsMap 
    in case connDirect of 0 -> stablishConnection targetCell (2, CantConnect) $ curriedMapInsert (cellProp {top = targetCell})
                          1 -> stablishConnection targetCell (3, CantConnect) $ curriedMapInsert (cellProp {right = targetCell})
                          2 -> stablishConnection targetCell (0, CantConnect) $ curriedMapInsert (cellProp {bottom = targetCell})
                          3 -> stablishConnection targetCell (1, CantConnect) $ curriedMapInsert (cellProp {left = targetCell})

initializeMazeAndGenerate :: Int -> [Int] -> Map.Map Cell CellProp
initializeMazeAndGenerate size randomList = generateMaze size randomCoords randomList $ getMapCellsWithGroup size 1 where randomCoords = shuffleList randomList $ getCellsCoordinate size

generateMaze :: Int -> [Cell] -> [Int] -> Map.Map Cell CellProp -> Map.Map Cell CellProp
generateMaze _ [] _ cellsMap = cellsMap
generateMaze  size (thisCell:cells) randList cellsMap = generateMaze size cells randTail updatedCellsMap 
    where updatedCellsMap = if (length possibleConn) /= 0 then
                changeGroup thisCell (snd toConnectCell) (stablishConnection thisCell toConnectCell cellsMap)
            else cellsMap
          toConnectCell = (possibleConn!!randNum)
          randNum = if length possibleConn <= 1 then 0 else divMod' (head randList) (length possibleConn - 1)
          randTail = if length randList == 0 then [] else tail randList
          possibleConn = searchPossibleConnection size cellsMap thisCell
          cellProp = lookupProp thisCell cellsMap

filterOnlyDifferentGroups :: Int ->  Map.Map Cell CellProp -> [Cell] -> [Cell]
filterOnlyDifferentGroups groupNum cellsMap cells = filter (\c -> groupNum /= (group (lookupProp c cellsMap))) cells 

changeGroup :: Cell -> Cell -> Map.Map Cell CellProp -> Map.Map Cell CellProp
changeGroup motherCell toConnectCell cellMaps = Map.union updatedElems cellMaps
    where updatedElems = Map.fromList $ map (\(cellIt, propIt) -> (cellIt, setGroupCellProp toGroup propIt)) cellsThatNeedUpdate
          cellsThatNeedUpdate = filter (\(_, propIt) -> (group propIt) == fromGroup) $ Map.toList cellMaps
          toGroup = group $ lookupProp motherCell cellMaps
          fromGroup = group $ lookupProp toConnectCell cellMaps

convertCellToRect :: Int -> Int -> Cell -> Int -> Rect
convertCellToRect thick dimen (Cell x y) side = let biggerDim = 2*thick+dimen
                                                    thickX = if x == 0 then 0 else thick 
                                                    thickY = if y == 0 then 0 else thick 
    in case side of 0 -> (Rect (Coord (x*dimen+x*thick-thickX) (y*dimen+y*thick-thickY)) (Dimen biggerDim thick))
                    1 -> (Rect (Coord (x*dimen+x*thick+dimen) (y*dimen+y*thick-thickY)) (Dimen thick biggerDim))
                    2 -> (Rect (Coord (x*dimen+x*thick-thickX) (y*dimen+y*thick+dimen)) (Dimen biggerDim thick))
                    3 -> (Rect (Coord (x*dimen+x*thick-thickX) (y*dimen+y*thick-thickY)) (Dimen thick biggerDim))

getAllRectForCell :: Cell -> CellProp -> Int -> Int ->[Rect]
getAllRectForCell cell cellProp thick dim = map (convertCellToRect thick dim cell) $ map fst $ getAbsentConnection cellProp [0..3]

convertCellsMapAsListToRects :: [(Cell, CellProp)] -> Int -> Int -> [Rect]
convertCellsMapAsListToRects ((cell, cellProp):[]) thick dim = getAllRectForCell cell cellProp thick dim
convertCellsMapAsListToRects (c:cellsList) thick dim = (convertCurried [c])++(convertCurried cellsList) 
    where convertCurried x = convertCellsMapAsListToRects x thick dim

createSvgFromCellsMap :: Int -> Int -> Int -> Map.Map Cell CellProp -> String
createSvgFromCellsMap thickWall dimenCell gridSize cellsMap = createSvgContent wSvg hSvg $ convertCellsMapAsListToRects (Map.toList cellsMap) thickWall dimenCell
    where hSvg = wSvg
          wSvg = gridSize*dimenCell+gridSize*thickWall+dimenCell+thickWall

generateMazeAndCreateSvg :: Int -> Int -> Int -> [Int] -> String
generateMazeAndCreateSvg thickWall dimenCell gridSize randomList = createSvgFromCellsMap thickWall dimenCell gridSize cellsMap
    where cellsMap = initializeMazeAndGenerate gridSize randomList