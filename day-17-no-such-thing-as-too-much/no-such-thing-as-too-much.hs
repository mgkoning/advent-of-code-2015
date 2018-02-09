import Data.Function
import Data.List
import Data.Ord

input = [33,14,18,20,45,35,16,35,1,13,18,13,50,44,48,6,24,41,30,42]

goodCombos target list = goodCombos' target list []
  where
    goodCombos' t l s
      | t < 0  = []
      | t == 0 = [s]
      | l == [] = []
      | otherwise = let (x:xs) = l in (goodCombos' (t-x) xs (x:s)) ++ (goodCombos' t xs s)

solve = do
  let allGoodCombos = goodCombos 150 input
  putStrLn "Part 1:"
  putStrLn $ show $ length allGoodCombos
  putStrLn "Part 2:"
  putStrLn $ show $ length $ head $ groupBy ((==) `on` length) $ sortBy (comparing length) allGoodCombos