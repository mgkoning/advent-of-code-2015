import Data.Map.Strict (Map)
import Data.Map.Strict (toAscList, fromList)
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)
import Data.List (foldl1)

target = 33100000
maxIter = 1000000
houseLimit = 50

type PresentsAtHouse = Map Int Int

presentsFromElf :: Int -> PresentsAtHouse
presentsFromElf n =
  let presents = 1 `seq` n*10 in fromList [(elf, presents) | elf <- [n,2*n..maxIter]]

presentsFromElf2 :: Int -> PresentsAtHouse
presentsFromElf2 n =
  let presents = 1 `seq` n*11 in fromList [(elf, presents) | elf <- take 50 [n,2*n..]]

mergePresents = merge preserveMissing preserveMissing (zipWithMatched sumPresents)
  where sumPresents _ x y = x + y

presents :: (Int -> PresentsAtHouse) -> PresentsAtHouse
presents presentsFn = foldl1 mergePresents $ map presentsFn [1..maxIter]

solveP1 = do
  putStrLn "Part 1:"
  let presentsList = toAscList $ presents presentsFromElf
  putStrLn $ show $ head $ dropWhile ((<target) . snd) presentsList
solveP2 = do
  putStrLn "Part 2:"
  let presentsList2 = toAscList $ presents presentsFromElf2
  putStrLn $ show $ head $ dropWhile ((<target) . snd) presentsList2
