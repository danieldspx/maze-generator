module Maze.Solver (
    getCellMapForShortestPathSolution
) where

import Maze.Generator
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import Debug.Trace as Deb

data Edge = Edge {vertice :: Cell, exitVertices :: [Cell]} deriving (Show)
data AStar = AStar {vertex :: Cell, distance :: Float, fromVertex :: Cell} deriving (Show)

type AStarMap = Map.Map Cell AStar
type EdgeMap = Map.Map Cell [Cell]

instance Eq AStar where
    as1 == as2 = (distance as1) == (distance as2)

instance Ord AStar where
    as1 < as2 = (distance as1) < (distance as2)
    as1 > as2 = (distance as1) > (distance as2)
    as1 <= as2 = (distance as1) < (distance as2) || as1 == as2
    as1 >= as2 = (distance as1) > (distance as2) || as1 == as2


valInfinity = 1/0

getEmptyAStart :: Cell -> AStar
getEmptyAStart c = AStar {vertex=c, distance=valInfinity, fromVertex=AbsentCell}

lookupExitVtx :: Cell -> EdgeMap -> [Cell]
lookupExitVtx x y = Maybe.fromMaybe [] $ Map.lookup x y

lookupAStar :: Cell -> AStarMap -> AStar
lookupAStar x y = Maybe.fromMaybe (getEmptyAStart x) $ Map.lookup x y

getNonAbsentCells :: CellProp -> [(Int, Cell)]
getNonAbsentCells cellProp = filter (\x -> (snd x) /= AbsentCell) $ map (\x -> (x, getConnection x cellProp)) [0..3]

getConnectedCells :: Cell -> CellProp -> Int -> [Cell]
getConnectedCells cell cellProp size = map (\(x, c) -> sameOrSearch c size x) nonAbsent
    where sameOrSearch c sz pos = if c == CantConnect then getNeighbourFromPos cell sz pos else c
          nonAbsent = getNonAbsentCells cellProp

cellsMapToEdge :: Map.Map Cell CellProp -> [Edge]
cellsMapToEdge cellsMap = map (\(cell, prop) -> Edge cell (getConnectedCells cell prop size)) cellList
    where cellList = Map.toList cellsMap
          size = (floor . sqrt . fromIntegral . length) cellsMap

createStartPoint :: Cell -> AStar
createStartPoint cell = AStar {vertex=cell, distance=0, fromVertex=AbsentCell}

initializeAStarWithInfinite :: Cell -> [Edge] -> [AStar]
initializeAStarWithInfinite initial edges = ((createStartPoint initial):) $ map fromEdgeToAStar fltrdEdges
    where fltrdEdges = filter (\edge -> (vertice edge) /= initial) edges
          fromEdgeToAStar edge = getEmptyAStart (vertice edge)

initializeShortestPath :: Map.Map Cell CellProp -> Cell -> Cell -> [Cell]
initializeShortestPath cellsMap from to = shortestPath to astarMap edgesMap [] keys
    where astarMap = Map.fromList $ map (\x -> (vertex x, x)) $ initializeAStarWithInfinite from edges
          edgesMap = Map.fromList $ map (\x -> ((vertice x), (exitVertices x))) edges
          edges = cellsMapToEdge cellsMap
          keys = Set.toList $ Map.keysSet cellsMap

distanceBtwCells :: Cell -> Cell -> Float
distanceBtwCells (Cell x1 y1) (Cell x2 y2) = sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2

updateAStarMap :: Cell -> Cell -> [Cell] -> AStarMap -> AStarMap
updateAStarMap _ _ [] astarMap = astarMap
updateAStarMap vertex toCell (extV:[]) astarMap = if dist < (distance extAStar) then Map.insert extV updatedAstar Map.empty else Map.empty
    where dist = originVtxDist + (distanceBtwCells extV toCell)
          updatedAstar = extAStar {distance=dist,fromVertex=vertex}
          extAStar = lookupAStar extV astarMap
          originVtxDist = distance $ lookupAStar vertex astarMap
updateAStarMap vertex toCell (extV:exitVertices) astarMap = Map.union mapAStarFromSingle mapAStarFromRest
    where mapAStarFromSingle = (updateAStarMap vertex toCell [extV] astarMap)
          mapAStarFromRest = (updateAStarMap vertex toCell exitVertices astarMap)

shortestPath :: Cell -> AStarMap -> EdgeMap -> [Cell] -> [Cell] -> [Cell]
shortestPath _ _ _ _ [] = error "All cells were visited and final cell was not found."
shortestPath toCell astarMap edgesMap visiteds unvisited = if currentVertex == toCell then reverseAStar toCell astarMap else callShortestPath
    where callShortestPath = shortestPath toCell updatedAstar edgesMap updatedVisteds updatedUnvisited
          currentVertex = fst $ (filter (\(c,_) -> not $ c `elem` visiteds) $ Map.toAscList astarMap)!!0
          exitVertices = filter (\x -> not $ x `elem` visiteds) $ lookupExitVtx currentVertex edgesMap
          updatedAstar = Map.union (updateAStarMap currentVertex toCell exitVertices astarMap) astarMap
          updatedVisteds = currentVertex:visiteds
          updatedUnvisited = List.delete currentVertex unvisited

reverseAStar :: Cell -> AStarMap -> [Cell]
reverseAStar AbsentCell _ = []
reverseAStar from astarMap = [from]++(reverseAStar reverseLinkCell astarMap)
    where reverseLinkCell = fromVertex $ lookupAStar from astarMap

getCellMapForShortestPathSolution :: Map.Map Cell CellProp -> Cell -> Cell -> Map.Map Cell CellProp
getCellMapForShortestPathSolution cellsMap from to = Map.mapWithKey (\key x -> if isPartOfSolution key then x {isSolution=True} else x) cellsMap
    where isPartOfSolution c = c `elem` cellsOfSolution
          cellsOfSolution = initializeShortestPath cellsMap from to
          