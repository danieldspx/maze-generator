import System.IO
import qualified ExampleMaze as Ex
import qualified Maze

mazeName = "maze.svg"

cellList = Maze.getCellList Ex.mazeCoords
sizeCell = 10

  
main = do  
    writeFile  mazeName $ Maze.getMazeSvg Ex.mazeSize sizeCell cellList