-- import System.IO

module ExampleMaze (
    mazeCoords,
    mazeSize
) where

mazeSize :: Int
mazeSize = 4

mazeCoords :: [(Int, Int)]
mazeCoords = [(0,0), (1,0), (2,0), (3,0),(3,1),(0,2), (1,2),(0,3), (1,3), (2,3), (3,3)]


