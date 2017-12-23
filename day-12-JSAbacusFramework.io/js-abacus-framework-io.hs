import Control.Applicative (empty)
import qualified Numeric as Numeric
import Text.ParserCombinators.Parsec
import Data.Either

data JValue = JObject [(String, JValue)] | JArray [JValue] | JString String | JInteger Int deriving (Show)

json :: CharParser () JValue
json = spaces *> text
       <?> "JSON text"
  where text = readObject <|> readArray

readObject :: CharParser () (JValue)
readObject = JObject <$> readSeries '{' readField '}'
  where readField = (,) <$> (readString <* char ':' <* spaces) <*> readValue

readArray :: CharParser () (JValue)
readArray = JArray <$> readSeries '[' readValue ']'

readSeries :: Char -> CharParser () a -> Char -> CharParser () [a]
readSeries left parser right =
  between (char left <* spaces) (char right) $
          (parser <* spaces) `sepBy` (char ',' <* spaces)

readValue :: CharParser () JValue
readValue = value <* spaces
  where value = choice [ JString <$> readString , JInteger <$> readInteger, readObject, readArray ] <?> "JSON value"

readInteger = do s <- getInput
                 case Numeric.readSigned Numeric.readDec s of
                   [(n, s')] -> n <$ setInput s'
                   _         -> empty

readString = between (char '\"') (char '\"') (many jchar)
  where jchar = satisfy (/= '\"')

calculateSum ignoreRed jValue =
  case jValue of
    JInteger x -> x
    JString x -> 0
    JArray xs -> sum $ map (calculateSum ignoreRed) xs
    JObject xs ->
      let skip = ignoreRed && (or $ map isRed xs)
          isRed (_, JString "red") = True
          isRed _ = False
      in if skip then 0 else sum $ map (calculateSum ignoreRed . snd) xs

solve = do
  json <- (parse json "input.txt") <$> readFile "input.txt"
  putStrLn "Part 1:"
  putStrLn $ show $ calculateSum False $ head $ rights [json]
  putStrLn "\nPart 2:"
  putStrLn $ show $ calculateSum True $ head $ rights [json]