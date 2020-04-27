
import qualified Maze.Generator as Maze
import System.IO
import System.Random

mazeName = "maze.svg"
dimenCell = 10
gridSize = 50
thickWall = 10

generateRandomNumbers :: Int -> Int -> Int -> StdGen -> [Int]
generateRandomNumbers from to total gen = take total $ (randomRs (from,to) gen)::([Int])

  
main = do
    gen <- getStdGen
    putStrLn "Generating maze..."
    writeFile  mazeName $ Maze.generateMazeAndCreateSvg thickWall dimenCell gridSize $ 
        generateRandomNumbers 1 (gridSize^2) (gridSize^2) gen
    putStrLn "Done."