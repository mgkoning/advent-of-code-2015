import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

readDistances :: String -> Map (String, String) Int
readDistances file = Map.fromList $ concatMap (readLine . words) $ lines file
  where
    readLine (from:_:to:_:d:[]) = 
      let distance = read d in [((from, to), distance), ((to, from), distance)]

shortestRoute :: Map (String, String) Int -> Int
shortestRoute = findRoute minimum

longestRoute :: Map (String, String) Int -> Int
longestRoute = findRoute maximum

findRoute :: ([Int] -> Int) -> Map (String, String) Int -> Int
findRoute aggregate distanceMap = aggregate $ map getDistance' $ permutations possibleDestinations
  where
    possibleDestinations = Set.toList $ Set.fromList $ map fst $ Map.keys distanceMap
    getDistance' xs = sum $ zipWith (\a b -> distanceMap ! (a, b)) xs (tail xs)

solve = do
  distanceMap <- readDistances <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ show $ shortestRoute distanceMap
  putStrLn "\nPart 2:"
  putStrLn $ show $ longestRoute distanceMap

testInput = "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"
testDistanceMap = readDistances testInput