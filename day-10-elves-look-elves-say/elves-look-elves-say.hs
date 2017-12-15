import Data.List

lookAndSay :: String -> [String]
lookAndSay val = let say = doSay' in (say):(lookAndSay say)
  where
    doSay = concatMap (\xs -> (show $ length xs) ++ (take 1 xs)) $ group val
    doSay' = concatMap (concat <$> ([show . length, take 1] <*>) <$> pure) $ group val

part1 = head . drop 39 . lookAndSay

part2 = head . drop 49 . lookAndSay