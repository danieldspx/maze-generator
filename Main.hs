
import qualified Maze.Generator as Generator
import qualified Maze.Solver as Solver
import System.IO
import System.Random

mazeName = "maze.svg"
mazeSolvedName = "maze_solution.svg"
dimenCell = 10
gridSize = 5
thickWall = 10
solutionCellStart = Generator.Cell 0 0
solutionCellEnd = Generator.Cell (gridSize-1) (gridSize-1)

generateRandomNumbers :: Int -> Int -> Int -> StdGen -> [Int]
generateRandomNumbers from to total gen = take total $ (randomRs (from,to) gen)::([Int])

  
main = do
        gen <- getStdGen
        let cellsMapMaze = Generator.initializeMazeAndGenerate gridSize $ generateRandomNumbers 1 (gridSize^2) (gridSize^2) gen
        putStrLn "Generating maze..."
        writeFile mazeName $ Generator.createSvgFromCellsMap thickWall dimenCell gridSize cellsMapMaze
        putStrLn "Generated."
        putStrLn "Solving maze..."
        let cellsMapSolution = Solver.getCellMapForShortestPathSolution cellsMapMaze solutionCellStart solutionCellEnd
        writeFile mazeSolvedName $ Generator.createSvgFromCellsMap thickWall dimenCell gridSize cellsMapSolution
        putStrLn "Done."