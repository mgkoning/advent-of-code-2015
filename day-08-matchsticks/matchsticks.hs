data Mode = Normal | Escape | HexEscapeFirst | HexEscapeSecond

stringLength :: String -> Int
stringLength s = count 0 Normal (tail (take (length s - 1) s))
  where
    count :: Int -> Mode -> String -> Int
    count n _ [] = n
    count n m (x:xs) =
      case m of
        Normal -> if x == '\\' then count (n+1) Escape xs else count (n+1) Normal xs
        Escape -> if x == 'x' then count n HexEscapeFirst xs else count n Normal xs
        HexEscapeFirst -> count n HexEscapeSecond xs
        HexEscapeSecond -> count n Normal xs

solve = do
  input <- readFile "input.txt"
  let inputLines = lines input
  let totalChars = sum (map length inputLines)
  let totalStringLength = sum (map stringLength inputLines)
  putStrLn "Part 1:"
  putStrLn $ show $ totalChars - totalStringLength
  putStrLn "Part 2:"
  putStrLn $ show $ sum (map (length . show) inputLines) - totalChars