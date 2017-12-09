import Data.List

data GiftDimensions = GiftDimensions { length :: Integer, width :: Integer, height :: Integer }

parseInput :: String -> [GiftDimensions]
parseInput s = map parseGift $ lines s

parseGift :: String -> GiftDimensions
parseGift s = dimensions parts
  where
    parts = map read $ words $ map (\x -> if x == 'x' then ' ' else x) s
    dimensions (l:w:h:[]) = GiftDimensions l w h
    dimensions _ = error "bad line"

paperNeeded :: GiftDimensions -> Integer
paperNeeded n = sum $ (head sides):pieces
  where
    pieces = sides ++ sides
    sides = sort [l*w, w*h, h*l]
    l = Main.length n
    w = width n
    h = height n

ribbonNeeded :: GiftDimensions -> Integer
ribbonNeeded n =  l*w*h + (sum $ take 4 (sort [l, l, w, w, h, h]))
  where
    l = Main.length n
    w = width n
    h = height n

solution = do
  input <- readFile "input.txt"
  let gifts = parseInput input
  putStrLn "Part 1:"
  putStrLn $ show $ sum $ map paperNeeded gifts
  putStrLn "\nPart 2:"
  putStrLn $ show $ sum $ map ribbonNeeded gifts