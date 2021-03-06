module Maze.Generator (
    generateMazeAndCreateSvg,
    Group,
    Cell(..),
    CellProp(..),
    getNeighbourFromPos,
    getConnection,
    initializeMazeAndGenerate,
    createSvgFromCellsMap
) where

import Svg
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace as Deb

type Group = Int
data Cell = AbsentCell | Cell Int Int | CantConnect deriving (Show, Eq, Ord) -- X Y
data CellProp = AbsentProp | CellProp {group::Group, top::Cell, right::Cell, bottom::Cell, left::Cell, isSolution::Bool} deriving (Show, Ord)
-- A CellProp with `group = 0` means that it has no group. A group must be >= 1.

instance Eq CellProp where
    cellP1 == cellP2 = (group cellP1) == (group cellP2) -- CellProp are equal when their group are.

divMod' :: Int -> Int -> Int
divMod' x = snd . divMod x

deleteNth :: Int -> [a] -> [a]
deleteNth i items = take i items ++ drop (1 + i) items

lookupProp :: Cell -> Map.Map Cell CellProp -> CellProp
lookupProp x y = Maybe.fromMaybe getEmptyCellProp $ Map.lookup x y

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
getEmptyCellProp = CellProp {group=0,top=AbsentCell,right=AbsentCell,bottom=AbsentCell,left=AbsentCell,isSolution=False}

getNeighbours :: Int -> Cell -> CellProp
getNeighbours _ AbsentCell = getEmptyCellProp
getNeighbours size cell = CellProp {group=0,top=(getTopNeighbour cell),
                                    right=(getRightNeighbour size cell),
                                    bottom=(getBottomNeighbour size cell),
                                    left=(getLeftNeighbour cell),
                                    isSolution=False}

setGroupTupleCells :: Int -> [(Cell, CellProp)] -> [(Cell, CellProp)]
setGroupTupleCells _ [] = [] 
setGroupTupleCells startGroup ((cell, cellProp):[]) = [(cell, cellProp {group=startGroup})]
setGroupTupleCells startGroup (x:xs) = (setGroupTupleCells startGroup [x])++(setGroupTupleCells (startGroup+1) xs)

setGroupMapCells :: Int -> Map.Map Cell CellProp -> Map.Map Cell CellProp
setGroupMapCells startGroup cells = Map.fromList $ setGroupTupleCells startGroup $ Map.toList cells

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

sameGroupHelper :: [(Cell, CellProp)] -> Int -> (Bool, Cell)
sameGroupHelper ((cell, prop):[]) defaultGroup = ((group prop) == defaultGroup, AbsentCell)
sameGroupHelper (it:rest) defaultGroup = if fst (sameGroupHelper [it] defaultGroup) then sameGroupHelper rest defaultGroup else (False, fst it)

areAllSameGroup :: Map.Map Cell CellProp -> (Bool, Cell)
areAllSameGroup cellsMap = sameGroupHelper (Map.toList cellsMap) $ group $ lookupProp (Cell 0 0) cellsMap

getAllFromSameGroup :: Int -> Map.Map Cell CellProp -> [Cell]
getAllFromSameGroup groupSearch cellsMap = map (\(cell,_) -> cell) $ filter (\(cell, prop) -> group prop == groupSearch) $ Map.toList cellsMap

generateMaze :: Int -> [Cell] -> [Int] -> Map.Map Cell CellProp -> Map.Map Cell CellProp
generateMaze size [] randList cellsMap = if fst areAllSameGroupResult then cellsMap else generateMaze size fromSameGp randList cellsMap
    where fromSameGp = getAllFromSameGroup (group (lookupProp (snd areAllSameGroupResult) cellsMap)) cellsMap
          areAllSameGroupResult = (areAllSameGroup cellsMap)
generateMaze size (thisCell:cells) randList cellsMap = generateMaze size cells randTail updatedCellsMap 
    where updatedCellsMap = if (length possibleConn) /= 0 then
                changeGroup thisCell (snd toConnectCell) (stablishConnection thisCell toConnectCell cellsMap)
            else cellsMap
          randListNonEmpty = if length randList == 0 then [0] else randList
          toConnectCell = (possibleConn!!randNum)
          randNum = if length possibleConn <= 1 then 0 else divMod' (head randList) (length possibleConn - 1)
          randTail = if length randListNonEmpty <= 1 then randListNonEmpty else tail randListNonEmpty
          possibleConn = searchPossibleConnection size cellsMap thisCell
          cellProp = lookupProp thisCell cellsMap

filterOnlyDifferentGroups :: Int ->  Map.Map Cell CellProp -> [Cell] -> [Cell]
filterOnlyDifferentGroups groupNum cellsMap cells = filter (\c -> groupNum /= (group (lookupProp c cellsMap))) cells 

changeGroup :: Cell -> Cell -> Map.Map Cell CellProp -> Map.Map Cell CellProp
changeGroup motherCell toConnectCell cellMaps = Map.union updatedElems cellMaps
    where updatedElems = Map.fromList $ map (\(cellIt, propIt) -> (cellIt, propIt {group=toGroup})) cellsThatNeedUpdate
          cellsThatNeedUpdate = filter (\(_, propIt) -> (group propIt) == fromGroup) $ Map.toList cellMaps
          toGroup = group $ lookupProp motherCell cellMaps
          fromGroup = group $ lookupProp toConnectCell cellMaps

convertCellToRect :: Int -> Int -> Cell -> Int -> Rect
convertCellToRect thick dimen (Cell x y) side = let biggerDim = 2*thick+dimen
    in case side of 0 -> (Rect (Coord (x*thick+x*dimen) (y*thick+y*dimen)) (Dimen biggerDim thick) "black")
                    1 -> (Rect (Coord ((x+1)*dimen+(x+1)*thick) (y*dimen+y*thick)) (Dimen thick biggerDim) "black")
                    2 -> (Rect (Coord (x*thick+x*dimen) ((y+1)*thick+(y+1)*dimen)) (Dimen biggerDim thick) "black")
                    3 -> (Rect (Coord (x*dimen+x*thick) (y*dimen+y*thick)) (Dimen thick biggerDim) "black")

getRectForSolution :: Int -> Int -> Cell -> Rect
getRectForSolution thick dimen (Cell x y) = Rect (Coord (padding+xMargin) (padding+yMargin)) (Dimen dimenSol dimenSol) "red"
    where dimenSol = floor $ 0.75*(fromIntegral dimen)
          padding = (dimen - dimenSol) `div` 2
          xMargin = x*thick+x*dimen + thick
          yMargin = y*thick+y*dimen + thick

getAllRectForCell :: Cell -> CellProp -> Int -> Int ->[Rect]
getAllRectForCell cell cellProp thick dim = solutionRect++allRects
    where allRects = map (convertCellToRect thick dim cell) $ map fst $ getAbsentConnection cellProp [0..3]
          solutionRect = if isSolution cellProp then [getRectForSolution thick dim cell] else []

convertCellsMapAsListToRects :: [(Cell, CellProp)] -> Int -> Int -> [Rect]
convertCellsMapAsListToRects ((cell, cellProp):[]) thick dim = getAllRectForCell cell cellProp thick dim
convertCellsMapAsListToRects (c:cellsList) thick dim = (convertCurried [c])++(convertCurried cellsList) 
    where convertCurried x = convertCellsMapAsListToRects x thick dim

createSvgFromCellsMap :: Int -> Int -> Int -> Map.Map Cell CellProp -> String
createSvgFromCellsMap thickWall dimenCell gridSize cellsMap = createSvgContent wSvg hSvg $ convertCellsMapAsListToRects (Map.toList cellsMap) thickWall dimenCell
    where hSvg = wSvg
          wSvg = (gridSize+1)*thickWall + gridSize*dimenCell

generateMazeAndCreateSvg :: Int -> Int -> Int -> [Int] -> String
generateMazeAndCreateSvg thickWall dimenCell gridSize randomList = createSvgFromCellsMap thickWall dimenCell gridSize cellsMap
    where cellsMap = initializeMazeAndGenerate gridSize randomList