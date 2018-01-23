import Data.Ord
import Data.List

type Speed = Int
type FlyTime = Int
type RestTime = Int

parseLines :: String -> [(String, Speed, FlyTime, RestTime)]
parseLines = map toReindeer . lines
  where
    -- Ex: can fly 7 km/s for 20 seconds, but then must rest for 119 seconds.
    toReindeer l = let name:_:_:speed:_:_:flyTime:_:_:_:_:_:_:restTime:_:[] = words l in (name, read speed, read flyTime, read restTime)

distances :: Int -> [(String, Speed, FlyTime, RestTime)] -> [(String, Int)]
distances time reindeer = map (distance time) reindeer

distance :: Int -> (String, Speed, FlyTime, RestTime) -> (String, Int)
distance time (name, speed, flyTime, restTime) = 
  let (fullCycles, timeLeft) = time `quotRem` (flyTime + restTime)
  in (name, speed * (fullCycles * flyTime + (min timeLeft flyTime)))

winnersAt time reindeer =
   let results = distances time reindeer
       maximumDistance = maximum $ map snd results
   in filter ((== maximumDistance) . snd) results 

solve = do
  reindeer <- parseLines <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ show $ winnersAt 2503 reindeer
  putStrLn "Part 2:"
  let winners = [1..2503] >>= (map fst . (`winnersAt` reindeer)) 
  let winner = maximumBy (comparing length) (group (sort winners))
  putStrLn $ show $ length winner
