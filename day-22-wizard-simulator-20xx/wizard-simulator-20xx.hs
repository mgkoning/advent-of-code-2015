import Prelude hiding (round)
import Data.List (foldl')

data Intent = Help | Harm deriving (Show, Eq)

data Effect = Effect { effectName :: String, effectDamage :: Int, effectArmor :: Int, effectManaReturn :: Int, roundsRemaining :: Int, intent :: Intent } deriving (Show)

data FighterState = FighterState { hp :: Int, fighterMana :: Int, fighterDamage :: Int, baseArmor :: Int, spellArmor :: Int, effects :: [Effect] } deriving (Show)

data Outcome = Win | Loss deriving (Show, Eq)

data Spell = Spell { spellName :: String, spellMana :: Int, spellDamage :: Int, spellHeal :: Int, spellEffect :: Maybe Effect } deriving Show

emptySpell = Spell "" 0 0 0 Nothing

{- the available spells -}
magicMissile = emptySpell { spellName = "magicMissile", spellMana = 53, spellDamage = 4 }
drain = emptySpell { spellName = "drain", spellMana = 73, spellDamage = 2, spellHeal = 2 }
shield = emptySpell { spellName = "shield", spellMana = 113, spellEffect = Just (Effect "shield" 0 7 0 6 Help) }
poison = emptySpell { spellName = "poison", spellMana = 173, spellEffect = Just (Effect "poison" 3 0 0 6 Harm) }
recharge = emptySpell { spellName = "recharge", spellMana = 229, spellEffect = Just (Effect "recharge" 0 0 101 5 Help) }

{- our main characters -}
player = FighterState 50 500 0 0 0 []
boss = FighterState 71 0 10 0 0 []

{- applies spell results including effects -}
cast spell boss player =
  let newBoss = boss { hp = hp boss - spellDamage spell }
      newPlayer = player { hp = hp player + spellHeal spell, fighterMana = fighterMana player - spellMana spell }
  in
  case spellEffect spell of
    Nothing -> (newBoss, newPlayer)
    Just effect ->
      case intent effect of
        Harm -> (newBoss { effects = effect:(effects newBoss) }, newPlayer)
        Help -> (newBoss, newPlayer { effects = effect:(effects player) })

{- is the player able to cast the spell -}
canCast boss player spell =
  if hp player < 0 then False -- actually a trick for part 2: can't cast if dead
  else if (fighterMana player) < (spellMana spell)
    then False
    else case spellEffect spell of
      Nothing -> True
      Just effect -> case intent effect of
        Harm -> not $ hasEffect effect boss
        Help -> not $ hasEffect effect player
  where hasEffect effect fighter =  effectName effect `elem` map effectName (effects fighter)

{- returns new fighter after applying all effects -}
applyEffects fighter =
  case effects fighter of
    [] -> fighter
    es -> foldl' applyEffect fighter es

{- returns new fighter after applying effect -}
applyEffect fighter effect =
  fighter {
    hp = (hp fighter) - (effectDamage effect),
    fighterMana = (fighterMana fighter) + (effectManaReturn effect),
    spellArmor = (spellArmor fighter) + effectArmor effect,
    effects = maybeCons (tick effect) (filter ((/= effectName effect) . effectName) $ effects fighter)
  }
  where
    {- determine new effect state -}
    tick effect =
      let rounds = (roundsRemaining effect) - 1
      in if rounds < 1
        then Nothing
        else Just effect { roundsRemaining = rounds }
    {- helper for conditional concatenation -}
    maybeCons x xs =
      case x of
        Nothing -> xs
        Just e -> e:xs

spells = [recharge, shield, poison, drain, magicMissile]

{- calculate all possible outcomes for the given input state -}
round :: FighterState -> FighterState -> Int -> [String] -> Int -> [(FighterState, FighterState, [String], Int)]
round boss player dotTick casts manaSpent =
  let player1 = applyEffects $ player { hp = hp player - dotTick }
      boss1 = applyEffects boss
      possibleCasts = filter (canCast boss1 player1) spells
      doCast boss player spell =
        let (boss2, player2) = cast spell boss player
            player3 = applyEffects $ player2 { spellArmor = 0 }
            boss3 = applyEffects boss2
            player4 = player3 { hp = takeDamage (hp player3) (fighterDamage boss3) ((baseArmor player3) + (spellArmor player3)) }
        in (boss3, player4, (spellName spell):casts, 1 `seq` manaSpent + (spellMana spell))
      takeDamage h d a = h - (max 1 (d - a))
  in map (doCast boss1 player1) possibleCasts

fight :: FighterState -> FighterState -> Int -> (Int, [(Outcome, ([String], Int))])
fight boss player dotTick =
  let fight' [] results minSpent = (minSpent, results)
      fight' ((boss, player, casts, manaSpent):outcomes) results minSpent =
        let
          (newRounds, newResults, newMinSpent) = 
            if hp boss <= 0 then ([], [(Win, (casts, manaSpent))], min manaSpent minSpent)
            else if minSpent <= manaSpent then ([], [], minSpent) -- don't bother: better solution found already
            else if hp player <= 0 then ([], [], minSpent)
            else (round boss player dotTick casts manaSpent, [], minSpent)
        {- put newRounds in front to find a possible solution faster, so we can start skipping calculations -}
        in fight' (newRounds ++ outcomes) (results ++ newResults) newMinSpent

  in fight' (round boss player dotTick [] 0) [] 100000

solve = do
  putStrLn "Part 1:"
  let (minSpent1, _) = fight boss player 0
  putStrLn $ show $ minSpent1
  putStrLn "Part 2:"
  let (minSpent2, _) = fight boss player 1
  putStrLn $ show $ minSpent2
