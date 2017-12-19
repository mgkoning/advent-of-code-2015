import Data.List
 
increment x = reverse $ increment' $ reverse x
  where
    increment' [] = error "overflow"
    increment' (c:cs) =
      let next = incrementChar c
      in if next > 'z' then 'a':(increment' cs) else next `seq` (next:cs)

incrementChar c =
  let next = toEnum $ (fromEnum c) + 1
  in if isBad next then incrementChar next else next

bad = or . map isBad

isBad = (flip elem) "iol"
 
pairs :: Eq a => [a] -> [[a]]
pairs = (filter ((>=2) . length)) . group
 
hasTwoDistinctPairs :: Eq a => [a] -> Bool
hasTwoDistinctPairs = (>1) . length . nub . concat . pairs
 
hasStraight a
  | length a < 3 = False
  | otherwise = let (b:c:d:rest) = a
                in if asc b c && asc c d then True else hasStraight (c:d:rest)

asc a b = (fromEnum b) - 1 == fromEnum a

generatePasswords start = filter goodPassword $ iterate increment start
  where goodPassword p = and $ [not . bad, hasStraight, hasTwoDistinctPairs] <*> [p]

solve = do
  putStrLn "Part 1:"
  let (p1:p2:_) = take 2 $ generatePasswords "hxbxwxba"
  putStrLn $ show $ p1
  putStrLn "\nPart 2:"
  putStrLn $ show $ p2

