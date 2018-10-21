import Data.Char (isUpper)
import qualified Data.Text as T
import Data.Text (pack)
import qualified Data.Set as Set
import Data.Set (Set, member, insert)
import Data.List (find, nub)

type Replacement = (T.Text, T.Text)

makeReplacement :: String -> String -> Replacement
makeReplacement from to = (pack from, pack to)

allReplacementsFor :: T.Text -> Replacement -> [T.Text]
allReplacementsFor input (from, to) =
  let breaks = T.breakOnAll from input
  in map (\(prefix, suffix) -> T.concat [prefix, to, T.drop (T.length from) suffix]) breaks

allReplacements :: [Replacement] -> T.Text -> [T.Text]
allReplacements replacements input = nub $ concatMap (allReplacementsFor input) replacements

findMolecule :: [Replacement] -> T.Text -> (Int, T.Text)
findMolecule replacements target =
  let findMolecule' :: [(Int, T.Text)] -> Set T.Text -> (Int, T.Text)
      findMolecule' [] _ = error "Not found"
      findMolecule' ((steps, molecule):xs) considered
        | molecule `member` considered = findMolecule' xs considered
        | otherwise =
          let possibilities = allReplacements replacements molecule
              incSteps = 1 `seq` 1 + steps
          in case find (==target) possibilities of
            Just x -> (incSteps, x)
            Nothing -> findMolecule' (xs ++ (zip (repeat incSteps) possibilities)) (molecule `insert` considered)
  in findMolecule' [(0, pack "e")] Set.empty

-- brute forcing this doesn't work at all. implemented this after cheating by looking at answer explanations.
actualSolution :: T.Text -> Int
actualSolution input =
  let elements = T.length $ T.filter isUpper input -- only upper case characters are relevant for counting 'elements'
      rnOrAr = T.count (pack "Ar") input + T.count (pack "Rn") input -- basically "parentheses"
      yCount = T.count (pack "Y") input -- basically "comma"
  in elements - rnOrAr - 2*yCount - 1

solve = do
  inputText <- pack <$> readFile "input.txt"
  replacements <- parseReplacements <$> readFile "replacements.txt"
  putStrLn "Part 1:"
  let possibilities = allReplacements replacements inputText
  putStrLn $ show $ length possibilities
  putStrLn "Part 2:"
  putStrLn $ show $ actualSolution inputText

parseReplacements :: String -> [Replacement]
parseReplacements input =
  let rs = lines input
      parseReplacement r = let (from, to) = T.breakOn (pack " => ") (pack r) in (from, (T.drop 4 to))
  in map parseReplacement rs

testReplacements = [makeReplacement "e" "H", makeReplacement "e" "O", makeReplacement "H" "HO", makeReplacement "H" "OH", makeReplacement "O" "HH"]
