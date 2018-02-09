import Data.List.Split

type Property = (String, Int)
type Sue = (Int, [Property])

analysisResults = [
    ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)
  ] :: [Property]

sues = (map parseSue) . lines

parseSue line =
  let _:num:[] = words $ takeWhile (/=':') line
      properties = map (toProperty . (splitOn ": ")) $ splitOn ", " $ drop 2 $ dropWhile (/=':') line
      toProperty (p:v:_) = (p, read v) :: Property
  in (read num, properties) :: Sue

matches isMatch properties sue = let (_, sueProperties) = sue in all (propertyMatches properties) sueProperties
  where propertyMatches properties actual = any (isMatch actual) properties

part1Match actual expected = actual == expected

part2Match (prop, value) expected
  | prop /= fst expected = False
  | prop == "cats" || prop == "trees" = value > snd expected
  | prop == "pomeranians" || prop == "goldfish" = value < snd expected
  | otherwise = value == snd expected

solve = do
  allSues <- sues <$> readFile "input.txt"
  let trueSues1 = filter (matches part1Match analysisResults) allSues
  putStrLn "Part 1:"
  putStrLn $ show trueSues1
  let trueSues2 = filter (matches part2Match analysisResults) allSues
  putStrLn "Part 1:"
  putStrLn $ show trueSues2