import Data.Ord
import Data.List

{-
Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
Butterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3
Chocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8
Candy: capacity 0, durability -1, flavor 0, texture 5, calories 8
-}

data Ingredient =
  Ingredient {
    capacity :: Int, durability :: Int, flavor :: Int, texture :: Int, calories :: Int
  } deriving (Show, Eq)

emptyIngredient = Ingredient 0 0 0 0 0
sprinkles = emptyIngredient { capacity = 2, flavor = -2 }
butterscotch = emptyIngredient { durability = 5, flavor = -3 }
chocolate = emptyIngredient { flavor = 5, texture = -1 }
candy = emptyIngredient { durability = -1, texture = 5 }

scores = [
  (sp, bu, ch, ca, score, totalCalories sp bu ch ca) |
    sp <- [1..100],
    bu <- [1..100],
    ch <- [1..100],
    ca <- [1..100],
    sp + bu + ch + ca == 100,
    let score = calculateScore sp bu ch ca,
    score > 0
  ]

calculateScore sp bu ch ca =
  let subscore prop (count, ingredient) = prop ingredient * count
      propscore is prop = max 0 (sum (map (subscore prop) is))
      spoons = zip [sp, bu, ch, ca] [sprinkles, butterscotch, chocolate, candy]
  in product $ map (propscore spoons) [capacity, durability, flavor, texture]

totalCalories sp bu ch ca = (sp + bu) * 3 + (ch + ca) * 8

solve = do
  putStrLn "Part 1:"
  putStrLn $ show $ maximumBy (comparing (\(_,_,_,_,x,_) -> x)) scores
  putStrLn "Part 2:"
  putStrLn $ show $ maximumBy (comparing (\(_,_,_,_,x,_) -> x)) (filter ((==500) . \(_,_,_,_,_,c) -> c) scores)