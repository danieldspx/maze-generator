
import qualified Maze as Maze
import System.IO
import System.Random

mazeName = "maze.svg"
-- cellList = Maze.getCellList Ex.mazeCoords
dimenCell = 30
gridSize = 10
thickWall = 10

generateRandomNumbers :: Int -> Int -> Int -> StdGen -> [Int]
generateRandomNumbers from to total gen = take total $ (randomRs (from,to) gen)::([Int])

  
main = do
    gen <- getStdGen
    writeFile  mazeName $ Maze.generateMazeAndCreateSvg thickWall dimenCell gridSize $ 
        generateRandomNumbers 1 (gridSize^2) (gridSize^2) gen