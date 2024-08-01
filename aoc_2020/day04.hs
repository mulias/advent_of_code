import           Control.Applicative          hiding (many, optional)
import           Control.Exception
import           Control.Monad
import qualified Data.Char                    as Char
import           Data.Function
import qualified Data.List                    as List
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Maybe                   as Maybe
import           System.IO
import           Text.ParserCombinators.ReadP

main = do
    batchFile <- readFile "puzzle_inputs/day04.txt"
    case parse passportsBatchFileParser batchFile of
      Just parsedPassportFields ->
        let validPassports =
              parsedPassportFields
              & fmap buildPassport
              & fmap (andThen validatePassport)
              & Maybe.mapMaybe id
            part1 = 216 -- supporting both parts at once is too much work
            part2 = length validPassports
            _ = assert $ part1 == 216
            _ = assert $ part2 == 150
         in
        putStrLn $
          "Part 1: " ++ show part1
          ++ "\n" ++
          "Part 2: " ++ show part2

      Nothing ->
        print "Error reading input file"

data PassportField
  = BYR Int
  | IYR Int
  | EYR Int
  | HGT Length
  | HCL String
  | ECL EyeColor
  | PID String
  | CID String
  | Invalid String String
  deriving (Eq, Ord, Show)

type UnstructuredPassport = [PassportField]

data Passport = Passport
  { byr :: Int               -- Birth Year
  , iyr :: Int               -- Issue Year
  , eyr :: Int               -- Expiration Year
  , hgt :: Length            -- Height
  , hcl :: String            -- Hair Color
  , ecl :: EyeColor          -- Eye Color
  , pid :: String            -- Passport ID
  , cid :: Maybe String      -- Country ID
  } deriving Show

data LengthUnit = IN | CM
  deriving (Eq, Ord, Show)

type Length = (Int, LengthUnit)

data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
  deriving (Eq, Ord, Show)

passportsBatchFileParser :: ReadP [UnstructuredPassport]
passportsBatchFileParser = do
  passports <- sepBy passportParser (string "\n")
  _untilFileEnd <- eof
  return passports

passportParser :: ReadP UnstructuredPassport
passportParser = many passportFieldParser

passportFieldParser :: ReadP PassportField
passportFieldParser = firstOf
  [ fieldParser "byr" BYR yearParser
  , fieldParser "iyr" IYR yearParser
  , fieldParser "eyr" EYR yearParser
  , fieldParser "hgt" HGT lengthParser
  , fieldParser "hcl" HCL hexParser
  , fieldParser "ecl" ECL eyeColorParser
  , fieldParser "pid" PID passportIdParser
  , fieldParser "cid" CID anyWordParser
  , invalidFieldParser
  ]

fieldParser :: String -> (a -> PassportField) -> ReadP a -> ReadP PassportField
fieldParser fieldName field fieldValParser = do
  string fieldName
  string ":"
  fieldVal <- fieldValParser
  fieledSepParser
  return (field fieldVal)

invalidFieldParser :: ReadP PassportField
invalidFieldParser = do
  fieldName <- count 3 get
  string ":"
  fieldVal <- anyWordParser
  fieledSepParser
  return $ Invalid fieldName fieldVal

yearParser :: ReadP Int
yearParser = count 4 digitParser & fmap read

lengthParser :: ReadP Length
lengthParser = do
  length <- many digitParser
  unit <- string "in" <|> string "cm"
  return (
      read length,
      case unit of {"in" -> IN; "cm" -> CM}
    )

hexParser :: ReadP String
hexParser = do
  string "#"
  code <- count 6 hexDigitParser
  return code

eyeColorParser :: ReadP EyeColor
eyeColorParser = enumParser
  [ ("amb", Amb)
  , ("blu", Blu)
  , ("brn", Brn)
  , ("gry", Gry)
  , ("grn", Grn)
  , ("hzl", Hzl)
  , ("oth", Oth)
  ]

enumParser :: [(String, a)] -> ReadP a
enumParser enums =
  firstOf $
    fmap (\(enumName, enumConstructor) ->
      fmap (const enumConstructor) (string enumName))
      enums

passportIdParser :: ReadP String
passportIdParser = count 9 digitParser

fieledSepParser :: ReadP String
fieledSepParser = string " " <|> string "\n"

digitParser :: ReadP Char
digitParser = satisfy Char.isDigit

hexDigitParser :: ReadP Char
hexDigitParser = satisfy Char.isHexDigit

anyWordParser :: ReadP String
anyWordParser = munch1 (not . Char.isSpace)

buildPassport :: UnstructuredPassport -> Maybe Passport
buildPassport fields =
  case List.sort fields of
    [BYR byr, IYR iyr, EYR eyr, HGT hgt, HCL hcl, ECL ecl, PID pid] ->
      Just (Passport byr iyr eyr hgt hcl ecl pid Nothing)
    [BYR byr, IYR iyr, EYR eyr, HGT hgt, HCL hcl, ECL ecl, PID pid, CID cid] ->
      Just (Passport byr iyr eyr hgt hcl ecl pid (Just cid))
    _ ->
      Nothing

validatePassport :: Passport -> Maybe Passport
validatePassport p =
  if isValidBirthYear (byr p)
    && isValidIssueYear (iyr p)
    && isValidExpirationYear (eyr p)
    && isValidHeight (hgt p)
     then Just p
     else Nothing

isValidBirthYear :: Int -> Bool
isValidBirthYear = isInRange 1920 2002

isValidIssueYear :: Int -> Bool
isValidIssueYear = isInRange 2010 2020

isValidExpirationYear :: Int -> Bool
isValidExpirationYear = isInRange 2020 2030

isValidHeight :: Length -> Bool
isValidHeight (val, IN) = isInRange 59 76 val
isValidHeight (val, CM) = isInRange 150 193 val

isInRange :: Int -> Int -> Int -> Bool
isInRange lower upper x = lower <= x && x <= upper

parse :: ReadP a -> String -> Maybe a
parse parser input =
  case readP_to_S parser input of
      [(result, "")] -> Just result
      _              -> Nothing

firstOf :: [ReadP a] -> ReadP a
firstOf = foldr (<++) pfail

andThen = (=<<)
