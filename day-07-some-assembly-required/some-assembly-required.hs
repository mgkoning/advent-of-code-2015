import Data.Bits
import Data.Char
import Data.Maybe
import Data.Word
import qualified Data.Map.Strict as Map

data Input = Constant Word16 | Noop String | Not String | Shift Input Int | And Input Input | Or Input Input deriving (Show, Eq)

getSides :: [String] -> (String, [String])
getSides x = (last x, takeWhile (/="->") x)

getInstructions :: String -> [(String, Input)]
getInstructions s =
  let instructions = map getSides $ map words $ lines s
      readInput i =
        case i of
          (x:[]) -> if all isDigit x then Constant (read x) else Noop x
          ("NOT":x:[]) -> Not x
          (x:op:y:[]) ->
            let inputX = (readInput [x])
                inputY = (readInput [y])
            in case op of
              "AND" -> And inputX inputY
              "OR" -> Or inputX inputY
              "LSHIFT" -> Shift inputX (read y)
              "RSHIFT" -> Shift inputX (negate (read y))
  in map (\(rhs, lhs) -> (rhs, readInput lhs)) instructions

isConstant (Constant _) = True
isConstant _ = False

valueFor :: (String, Input) -> Map.Map String Word16 -> Maybe Word16
valueFor (output, input) valueMap =
  case input of
    Constant x -> Just x
    Noop x -> Map.lookup x valueMap
    Not x -> applyUnary complement (Map.lookup x valueMap)
    Shift (Noop x) y -> applyUnary (`shift` y) (Map.lookup x valueMap)
    And (Constant x) (Noop y) -> applyBinary (.&.) (Just x) (Map.lookup y valueMap)
    And (Noop x) (Noop y) -> applyBinary (.&.) (Map.lookup x valueMap) (Map.lookup y valueMap)
    Or (Noop x) (Noop y) -> applyBinary (.|.) (Map.lookup x valueMap) (Map.lookup y valueMap)

applyUnary :: (a -> a) -> Maybe a -> Maybe a
applyUnary f (Just x) = Just (f x)
applyUnary _ _ = Nothing

applyBinary :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
applyBinary f (Just x) (Just y) = Just (f x y)
applyBinary _ _ _ = Nothing

buildValueMap :: String -> Map.Map String Word16
buildValueMap s = 
  let instructions = getInstructions s
      initialMap = Map.fromList $ map (\(x, Constant y) -> (x, y)) $ filter (isConstant . snd) instructions
      buildValueMap' m [] = m
      buildValueMap' m ((o, i):is) =
        let value = valueFor (o, i) m
        in if (isNothing value) then buildValueMap' m (is ++ [(o, i)])
           else buildValueMap' (Map.insert o (fromJust value) m) is
  in buildValueMap' initialMap (filter (not . isConstant . snd) instructions)

solve = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  let answerPart1 = (buildValueMap input) Map.! "a"
  putStrLn $ show answerPart1
  putStrLn "\nPart 2:"
  putStrLn $ show $ (buildValueMap (input ++ "\n" ++ (show answerPart1) ++ " -> b")) Map.! "a"

testInput = "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"