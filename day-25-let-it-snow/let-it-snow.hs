import Data.List (foldl')
next (r, c) = if r - 1 < 1 then (c+1, 1) else (r-1, c+1)

target = (2978, 3083)

multiplyBy = 252533 :: Integer
divideBy = 33554393 :: Integer
startAt = 20151125 :: Integer

indexes :: (Integer, Integer) -> [(Integer, Integer)]
indexes from = from : (indexes $! (next from))

nextCode :: Integer -> Integer
nextCode previousCode = (previousCode * multiplyBy) `rem` divideBy

allIndexes :: (Integer, Integer) -> [(Integer, Integer)]
allIndexes target = takeWhile ((/=)target) $ indexes (1,1)

{- ### Today's lesson: foldl' is better than writing your own recursion ### -}
code :: (Integer, Integer) -> Integer
code target = foldl' (\x _ -> nextCode x) startAt $ allIndexes target

