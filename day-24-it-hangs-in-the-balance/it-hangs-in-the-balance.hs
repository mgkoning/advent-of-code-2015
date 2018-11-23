import Data.List (sortOn)

packages = [1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113]

neededWeight1 = sum packages `quot` 3
neededWeight2 = sum packages `quot` 4

pick n xs = 
  let
    pick' :: Int -> [Int] -> [[Int]] -> [[Int]]
    pick' 0 _ lists = lists
    pick' n [] lists = []
    pick' n (x:xs) lists = pick' (n `seq` n-1) xs (map (x:) lists) ++ pick' n xs lists
  in pick' n xs [[]]

minPackages neededWeight = fst $ head $ filter ((>0) . length . snd) $ map (\n -> (n, filter ((==neededWeight) . sum) $ pick n packages)) [1..]

solve = do
  putStrLn "Part 1:"
  let minPackages1 = minPackages neededWeight1
  putStrLn $ show $ product $ head $ sortOn product $ filter ((==neededWeight1) . sum) $ pick minPackages1 packages
  putStrLn "Part 2:"
  let minPackages2 = minPackages neededWeight2
  putStrLn $ show $ product $ head $ sortOn product $ filter ((==neededWeight2) . sum) $ pick minPackages2 packages
