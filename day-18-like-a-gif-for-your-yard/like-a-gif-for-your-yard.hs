import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, lookup, (!), elems)
import qualified Data.Maybe as Maybe
import Data.Maybe (Maybe, catMaybes)
import Prelude hiding (lookup)

data LightState = On | Off deriving (Show, Eq)

type LightGrid = Map (Int, Int) LightState

lightGridFromString :: String -> LightGrid
lightGridFromString text =
  let state x = if x == '.' then Off else On
  in Map.fromList [((x, y), state s) | (y, line) <- zip [0..] $ lines text,
                                       (x, s) <- zip [0..] line]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [ (x + x', y + y') | x' <- [-1, 0, 1],
                                        y' <- [-1, 0, 1],
                                        x' /= 0 || y' /= 0 ]

neighborLights :: LightGrid -> (Int, Int) -> [LightState]
neighborLights grid xy = catMaybes $ map (`lookup` grid) $ neighbors xy

newStateAt1 :: (Int, Int) -> LightGrid -> LightState
newStateAt1 xy grid = nextLightState (grid ! xy) (neighborLights grid xy)

newStateAt2 :: (Int, Int) -> LightGrid -> LightState
newStateAt2 xy grid = case xy of
  (0, 0) -> On
  (0, 99) -> On
  (99, 0) -> On
  (99, 99) -> On
  _ -> nextLightState (grid ! xy) (neighborLights grid xy)

nextLightState :: LightState -> [LightState] -> LightState
nextLightState now neighbors =
  let onNeighbors = length $ filter (== On) neighbors
  in case (now, onNeighbors) of
    (On, 2) -> On
    (On, 3) -> On
    (Off, 3) -> On
    _ -> Off

newState :: LightGrid -> ((Int, Int) -> LightGrid -> LightState) -> LightGrid
newState grid newStateFn = Map.mapWithKey (\xy _ -> newStateFn xy grid) grid

stateAfterSteps :: LightGrid -> Int -> ((Int, Int) -> LightGrid -> LightState) -> LightGrid
stateAfterSteps grid n newStateFn = last $ take (n + 1) $ scanl (\g _ -> newState g newStateFn) grid [0..]

lightsOn :: LightGrid -> Int
lightsOn grid = length $ filter (== On) $ elems grid

test = do
  let grid1 = Map.fromList [((0,0), On), ((0,1), Off), ((1,0), On), ((1,1), On), ((0,2), Off), ((1,2), On)]
  putStrLn $ show $ newStateAt1 (0, 0) grid1
  let grid2 = Map.fromList [((0,0), On), ((0,1), Off), ((1,0), On), ((1,1), Off)]
  putStrLn $ show $ newStateAt1 (0, 0) grid2
  let evolve = newState grid1 newStateAt1
  putStrLn $ show $ evolve
  putStrLn $ show $ newState evolve newStateAt1

testInput = ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
testGrid = lightGridFromString testInput

solve = do
  text <- readFile "input.txt"
  let grid = lightGridFromString text
  putStrLn "Part 1"
  putStrLn $ show $ lightsOn $ stateAfterSteps grid 100 newStateAt1
  putStrLn "Part 1"
  putStrLn $ show $ lightsOn $ stateAfterSteps grid 100 newStateAt2