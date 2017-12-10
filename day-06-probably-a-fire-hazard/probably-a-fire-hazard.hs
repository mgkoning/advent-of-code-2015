import Data.Function
import Data.List
import qualified Data.Map.Strict as Map

makeInitialState :: Map.Map (Int, Int) Int
makeInitialState = Map.fromList [((x, y), 0) | x <- [0..999], y <- [0..999]]

makeNextState :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Map.Map (Int, Int) Int -> [String] -> Map.Map (Int, Int) Int
makeNextState toggle turnOn turnOff state instruction =
  case instruction of
    ("toggle":from:"through":through:[]) ->
      let (fromX, fromY) = readCoord from
          (toX, toY) = readCoord through
          range = [(x, y) | x <- [fromX..toX], y <- [fromY..toY]]
      in foldl' (flip (Map.adjust toggle)) state range
    ("turn":lightState:from:"through":through:[]) ->
      let (fromX, fromY) = readCoord from
          (toX, toY) = readCoord through
          range = [(x, y) | x <- [fromX..toX], y <- [fromY..toY]]
          op = if lightState == "on" then turnOn else turnOff
      in foldl' (flip (Map.adjust op)) state range

readCoord :: String -> (Int, Int)
readCoord s = let (a, b) = span (/=',') s in (read a, read $ tail b)

runInstructions1 :: [[String]] -> Map.Map (Int, Int) Int
runInstructions1 = foldl' (makeNextState (abs . subtract 1) (const 1) (const 0)) makeInitialState 

runInstructions2 :: [[String]] -> Map.Map (Int, Int) Int
runInstructions2 = foldl' (makeNextState (+2) (+1) (max 0 . subtract 1)) makeInitialState 

solve = do
  input <- readFile "input.txt"
  let instructions = map words $ lines input
  let result = runInstructions1 instructions
  putStrLn "Part 1:"
  putStrLn $ show $ length $ filter (>0) $ Map.elems result
  putStrLn "\nPart 2:"
  putStrLn $ show $ sum $ Map.elems $ runInstructions2 instructions