import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

makeHappinessMap = Map.fromList . (map readLine) . lines
  where
    readLine :: String -> ((String, String), Int)
    readLine s = let one:_:sign:value:_:_:_:_:_:_:other:[] = words s in ((one, init other), (if sign == "lose" then negate else id) (read value))

determineOptimalSeatingValue happinessMap extras = optimum
  where
    participants = extras ++ (Set.toList $ Set.fromList (map fst (Map.keys happinessMap)))
    optimum = maximum $ map totalHappiness $ permutations $ participants
    totalHappiness ps = sum $ zipWith happiness ps (drop 1 ps ++ take 1 ps)
    happiness a b = if a == "Me" || b == "Me" then 0 else happinessMap Map.! (a, b) + happinessMap Map.! (b, a)

solve = do
  happinessMap <- makeHappinessMap <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ determineOptimalSeatingValue happinessMap []
  putStrLn "Part 2:"
  print $ determineOptimalSeatingValue happinessMap ["Me"]