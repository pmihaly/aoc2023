module Main where

import Data.Attoparsec (Parser, count, option, parseOnly, string)
import Data.Attoparsec.ByteString.Char8 (char, decimal, digit, endOfLine, space)
import Data.Text (unpack)
import Data.Time (LocalTime (LocalTime, localDay, localTimeOfDay), TimeOfDay (TimeOfDay), fromGregorian)
import Relude.Extra (safeToEnum)

data IP = IP Word8 Word8 Word8 Word8 deriving (Show)

parseIP :: Parser IP
parseIP = IP <$> decimal <* char '.' <*> decimal <* char '.' <*> decimal <* char '.' <*> decimal

-- parseIP = do
--   d1 <- decimal
--   _ <- char '.'
--   d2 <- decimal
--   _ <- char '.'
--   d3 <- decimal
--   _ <- char '.'
--   IP d1 d2 d3 <$> decimal

--- >>> parseOnly parseIP "131.45.68.123"
-- Right (IP 131 45 68 123)

data Product = Mouse | Keyboard | Monitor | Speakers deriving (Bounded, Enum, Show)

productParser :: Parser Main.Product
productParser =
  (string "mouse" >> return Mouse)
    <|> (string "keyboard" >> return Keyboard)
    <|> (string "monitor" >> return Monitor)
    <|> (string "speakers" >> return Speakers)

productToId :: Main.Product -> Int
productToId = fromEnum >>> (+ 1)

productFromId :: Int -> Maybe Main.Product
productFromId = subtract 1 >>> safeToEnum

--- >>> productFromId $ productToId Speakers
-- Speakers

productIdParser :: Parser Main.Product
productIdParser = do
  result <- readEither <$> (digit <&> (: []))
  let productId = case result of
        Left err -> fail $ unpack err
        Right productId' -> productFromId productId'

  case productId of
    Nothing -> fail "invalid product id"
    Just p -> return p

--- >>> parseOnly productIdParser "2"
-- Right Keyboard

timeParser :: Parser LocalTime
timeParser = do
  y <- count 4 digit
  _ <- char '-'
  mm <- count 2 digit
  _ <- char '-'
  d <- count 2 digit
  _ <- space
  h <- count 2 digit
  _ <- char ':'
  m <- count 2 digit
  _ <- char ':'
  s <- count 2 digit

  let parseResult = do
        year <- readEither y
        month <- readEither mm
        day <- readEither d
        hour <- readEither h
        minute <- readEither m
        seconds <- readEither s

        return $
          LocalTime
            { localDay = fromGregorian year month day,
              localTimeOfDay = TimeOfDay hour minute seconds
            }

  case parseResult of
    Left err -> fail $ unpack err
    Right lt -> return lt

-- >>> parseOnly ("foo" *> "bar") "foobar"
-- Right "bar"

europeanTimeParser :: Parser LocalTime
europeanTimeParser = do
  d <- count 2 digit
  _ <- char '/'
  mm <- count 2 digit
  _ <- char '/'
  y <- count 4 digit
  _ <- space
  h <- count 2 digit
  _ <- char ':'
  m <- count 2 digit
  _ <- char ':'
  s <- count 2 digit

  let parseResult = do
        year <- readEither y
        month <- readEither mm
        day <- readEither d
        hour <- readEither h
        minute <- readEither m
        seconds <- readEither s

        return $
          LocalTime
            { localDay = fromGregorian year month day,
              localTimeOfDay = TimeOfDay hour minute seconds
            }

  case parseResult of
    Left err -> fail $ unpack err
    Right lt -> return lt

-- >>> parseOnly ("foo" *> "bar") "foobar"
-- Right "bar"

data Source = Internet | Friend | NoAnswer deriving (Show)

sourceParser :: Parser Source
sourceParser = (string "internet" >> return Internet) <|> (string "friend" >> return Friend) <|> (string "noanswer" >> return NoAnswer)

-- >>> parseOnly timeParser "2002-12-06 12:45:23"
-- Right 2002-12-06 12:45:23

data LogEntry = LogEntry
  { -- A local time contains the date and the time of the day.
    -- For example: 2013-06-29 11:16:23.
    entryTime :: LocalTime,
    entryIP :: IP,
    entryProduct :: Main.Product,
    source :: Source
  }
  deriving (Show)

logEntryParser :: Parser LogEntry
logEntryParser = LogEntry <$> timeParser <* space <*> parseIP <* space <*> productParser <*> option NoAnswer (space >> sourceParser)

-- >>> parseOnly logEntryParser "2013-06-29 11:16:23 192.168.0.24 keyboard"
-- Right (LogEntry {entryTime = 2013-06-29 11:16:23, entryIP = IP 192 168 0 24, entryProduct = Keyboard, source = NoAnswer})

europeanLogEntryParser :: Parser LogEntry
europeanLogEntryParser = do
  ip <- parseIP
  _ <- space
  time <- europeanTimeParser
  _ <- space
  product' <- productIdParser
  source' <- option NoAnswer (space >> sourceParser)
  return $ LogEntry time ip product' source'

-- >>> parseOnly europeanLogEntryParser  "154.41.32.99 29/06/2013 15:32:23 4 internet"
-- Right (LogEntry {entryTime = 2013-06-29 15:32:23, entryIP = IP 154 41 32 99, entryProduct = Speakers, source = Internet})

type Log = [LogEntry]

logParser :: Parser Log
logParser = many $ (logEntryParser <|> europeanLogEntryParser) <* endOfLine

-- >>> parseOnly logParser "2013-06-29 11:16:23 192.168.0.24 keyboard internet\n2013-06-29 11:16:24 192.168.0.12 mouse friend\n2013-06-31 11:16:24 192.168.0.83 monitor\n154.41.32.99 29/06/2013 15:32:23 4 internet\n2013-06-30 11:16:24 192.168.0.12 touchpad noanswer\n"
-- Right [LogEntry {entryTime = 2013-06-29 11:16:23, entryIP = IP 192 168 0 24, entryProduct = Keyboard, source = Internet},LogEntry {entryTime = 2013-06-29 11:16:24, entryIP = IP 192 168 0 12, entryProduct = Mouse, source = Friend},LogEntry {entryTime = 2013-06-30 11:16:24, entryIP = IP 192 168 0 83, entryProduct = Monitor, source = NoAnswer},LogEntry {entryTime = 2013-06-29 15:32:23, entryIP = IP 154 41 32 99, entryProduct = Speakers, source = Internet}]

main :: IO ()
main = readFileBS "app/logfile.log" >>= print . parseOnly logParser
