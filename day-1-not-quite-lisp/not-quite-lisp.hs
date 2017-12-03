
destination :: [Char] -> Integer
destination = sum . instructions

instructions :: [Char] -> [Integer]
instructions = map (\x -> if x == '(' then 1 else -1)

whenAtBasement :: [Char] -> Integer
whenAtBasement xs = whenAtBasement' (instructions xs) 0 0
  where
    whenAtBasement' :: [Integer] -> Integer -> Integer -> Integer
    whenAtBasement' xs index pos
      | pos == -1 = index
      | xs == [] = error "Never got there"
      | otherwise = whenAtBasement' xs' (index + 1) (pos + x)
        where x:xs' = xs

solution = do
    input <- readFile "input.txt"
    putStrLn "Part 1:"
    putStrLn $ show $ destination input
    putStrLn "\nPart 2:"
    putStrLn $ show $ whenAtBasement ")"
    putStrLn $ show $ whenAtBasement "()())"
    putStrLn $ show $ whenAtBasement input