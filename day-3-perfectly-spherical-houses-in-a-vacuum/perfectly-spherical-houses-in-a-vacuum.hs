import Data.List

locations :: [Char] -> [(Int, Int)]
locations input = locations' (0,0) input [(0, 0)]
  where
    locations' _ [] visited = nub visited
    locations' (x, y) (z:zs) visited = locations' nextPos zs $ nextPos:visited
      where nextPos = case z of '<' -> (x-1,y)
                                '>' -> (x+1,y)
                                '^' -> (x,y+1)
                                'v' -> (x,y-1)
      
divide :: [Char] -> ([Char], [Char]) -> ([Char], [Char])
divide [] (as, bs) = (reverse as, reverse bs)
divide (a:[]) (as, bs) = (reverse (a:as), reverse bs)
divide (a:b:rest) (as, bs) = divide rest (a:as, b:bs)

locationsWithDivide :: [Char] -> [(Int, Int)]
locationsWithDivide input = nub (santaHouses ++ roboHouses)
  where
    santaHouses = locations santa
    roboHouses = locations robo
    (santa, robo) = divide input ([],[])

solution = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ "^>v<: " ++ (show $ length $ locations "^>v<")
  putStrLn $ show $ length $ locations input
  putStrLn "\nPart 2:"
  putStrLn $ "^v: " ++ (show $ length $ locationsWithDivide "^v")
  putStrLn $ "^v^v^v^v^v: " ++ (show $ length $ locationsWithDivide "^v^v^v^v^v")
  putStrLn $ show $ length $ locationsWithDivide input
