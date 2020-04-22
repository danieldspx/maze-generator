import System.IO  
  
main = do  
    handle <- openFile "coordinates.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle