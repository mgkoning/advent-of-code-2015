import Data.List (subsequences, sortOn)

data FighterState = FighterState { hp :: Int, fighterDamage :: Int, fighterArmor :: Int } deriving (Show)

data Item = Item { name:: String, cost :: Int, itemDamage :: Int, itemArmor :: Int } deriving (Show)

data PlayerGear = PlayerGear { items:: [Item], gearCost :: Int, gearDamage :: Int, gearArmor :: Int } deriving (Show)

data Outcome = Win | Loss deriving (Show, Eq)

weapons = [Item "Dagger" 8 4 0,
           Item "Shortsword" 10 5 0,
           Item "Warhammer" 25 6 0,
           Item "Longsword" 40 7 0,
           Item "Greataxe" 74 8 0]

armors = [Item "Leather" 13 0 1,
          Item "Chainmail" 31 0 2,
          Item "Splintmail" 53 0 3,
          Item "Bandedmail" 75 0 4,
          Item "Platemail" 102 0 5] 

rings = [Item "Ring of Damage +1" 25 1 0,
         Item "Ring of Damage +2" 50 2 0,
         Item "Ring of Damage +3" 100 3 0,
         Item "Ring of Defense +1" 20 0 1,
         Item "Ring of Defense +2" 40 0 2,
         Item "Ring of Defense +3" 80 0 3]

possibleGear = [g | w <- subsequences weapons, length w == 1,
                    a <- subsequences armors, length a < 2,
                    r <- subsequences rings, length r < 3,
                    let items = w ++ a ++ r,
                    let g = PlayerGear items (sum $ map cost items) (sum $ map itemDamage items) (sum $ map itemArmor items)]

orderedGear = sortOn gearCost possibleGear

bossHP = 103
bossDamage = 9
bossArmor = 2
initialBossState = FighterState bossHP bossDamage bossArmor

fightRound :: FighterState -> FighterState -> Outcome
fightRound player boss
  | hp newBossState < 1 = Win
  | hp newPlayerState < 1 = Loss
  | otherwise = fightRound newPlayerState newBossState
  where takeDamage h d a = h - (max 1 (d - a))
        fight attacker defender = defender { hp = takeDamage (hp defender) (fighterDamage attacker) (fighterArmor defender) }
        newBossState = fight player boss
        newPlayerState = fight boss player

doFight :: PlayerGear -> Outcome
doFight g = fightRound (initPlayer g) initialBossState

initPlayer :: PlayerGear -> FighterState
initPlayer gear = FighterState 100 (gearDamage gear) (gearArmor gear)

solve = do
  putStrLn "Part 1:"
  putStrLn $ show $ head $ dropWhile ((==Loss) . doFight) orderedGear
  putStrLn "Part 2:"
  putStrLn $ show $ head $ dropWhile ((==Win) . doFight) $ reverse orderedGear