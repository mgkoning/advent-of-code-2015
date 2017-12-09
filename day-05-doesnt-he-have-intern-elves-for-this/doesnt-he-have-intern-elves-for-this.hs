import Data.List

data Niceness = Nice String | Naughty String String deriving Show

naughtyOrNice :: String -> Niceness
naughtyOrNice s
  | or (map (`isInfixOf` s) ["ab", "cd", "pq", "xy"]) = Naughty s "Contains forbidden sequence"
  | (length $ filter (`elem` "aeiou") s) < 3 = Naughty s "Not enough vowels"
  | not $ or $ zipWith (==) s (tail s) = Naughty s "Missing double letter"
  | otherwise = Nice s

naughtyOrNice2 :: String -> Niceness
naughtyOrNice2 s
  | not $ hasRepeatingPair s = Naughty s "Missing repeating pair"
  | not $ or $ zipWith3 (\a b c -> a == c) s (tail s) (drop 2 s) = Naughty s "Missing repeat with gap"
  | otherwise = Nice s
    where
      hasRepeatingPair (a:b:rest)
        | (a:b:[]) `isInfixOf` rest = True
        | otherwise = hasRepeatingPair (b:rest)
      hasRepeatingPair _ = False


isNice :: Niceness -> Bool
isNice (Nice x) = True
isNice _ = False

solution :: IO()
solution = do
  input <- readFile "input.txt"
  let words = lines input
  putStrLn "Part 1: "
  putStrLn $ show $ naughtyOrNice "ugknbfddgicrmopn"
  putStrLn $ show $ naughtyOrNice "aaa"
  putStrLn $ show $ naughtyOrNice "jchzalrnumimnmhp"
  putStrLn $ show $ naughtyOrNice "haegwjzuvuyypxyu"
  putStrLn $ show $ naughtyOrNice "dvszwmarrgswjxmb"
  let (nice, naughty) = partition isNice $ map naughtyOrNice words
  putStr "Nice words: "
  putStrLn $ show $ length nice
  putStrLn "\nPart 2: "
  putStrLn $ show $ naughtyOrNice2 "qjhvhtzxzqqjkmpb"
  putStrLn $ show $ naughtyOrNice2 "xxyxx"
  putStrLn $ show $ naughtyOrNice2 "uurcxstgmygtbstg"
  putStrLn $ show $ naughtyOrNice2 "ieodomkazucvgmuy"
  let (nice2, naughty2) = partition isNice $ map naughtyOrNice2 words
  putStr "Nice words: "
  putStrLn $ show $ length nice2

{- Output:

Part 1:
Nice "ugknbfddgicrmopn"
Nice "aaa"
Naughty "jchzalrnumimnmhp" "Missing double letter"
Naughty "haegwjzuvuyypxyu" "Contains forbidden sequence"
Naughty "dvszwmarrgswjxmb" "Not enough vowels"
Nice words: <answer part 1>

Part 2:
Nice "qjhvhtzxzqqjkmpb"
Nice "xxyxx"
Naughty "uurcxstgmygtbstg" "Missing repeat with gap"
Naughty "ieodomkazucvgmuy" "Missing repeating pair"
Nice words: <answer part 2>

-}