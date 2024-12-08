import Text.Parsec
import Text.Parsec.String (Parser)
-- import Data.Void
-- type Parser = Parsec Void String 

--JSON can be null, bool, number, string, array or object
data JSON = JsonNull 
          | JsonBool Bool 
          | JsonNumber Int 
          | JsonString String 
          | JsonArray [JSON] 
          | JsonObject [(String, JSON)]  --{"key":value}
          deriving (Show, Eq)

-- class Applicative f => Alternative f where
--     empty :: f a 
--     (<|>) :: f a -> f a -> f a 
    
jsonParse :: Parser JSON 
jsonParse = parseNull 
          <|> parseBool
          <|> parseNumber 
          <|> parseString
          <|> parseArray 
          <|> parseObject


parseNull :: Parser JSON 
parseNull = string "null" >> return JsonNull 

parseBool :: Parser JSON
parseBool = (string "true" >> return (JsonBool True))
       <|> (string "false" >> return (JsonBool False))


parseNumber :: Parser JSON 
parseNumber = JsonNumber <$> (read <$> many1 digit) 

parseString :: Parser JSON 
parseString = JsonString <$> (char '"' *> many (noneOf "\"") <* char '"')

parseArray :: Parser JSON 
parseArray = JsonArray <$> between (char '[' ) (char ']') (sepBy jsonParse (char ','))

parseObject :: Parser JSON 
parseObject = JsonObject <$> (char '{' *> sepBy key_value (char ',') <* char '}')
  where
    key_value = (,) <$> (char '"' *> manyTill anyChar (char '"')) <*> (char ':' *> jsonParse)

runParser :: String -> Either ParseError JSON
runParser input = parse jsonParse "" input

--runParser "[1,2,{\"Thalia\":\"Atallah\"}]"
-- Right (JsonArray [JsonNumber 1,JsonNumber 2,JsonObject [("Thalia",JsonString "Atallah")]])
-- Right yaane zabat l parser 
-- Left yaane failed 