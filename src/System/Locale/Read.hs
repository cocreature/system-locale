{-# LANGUAGE DeriveDataTypeable #-}
{-| Read locales on unix systems and parse them into their corresponding 'TimeLocale' representation.
-}
module System.Locale.Read
  ( getLocale
  , getCurrentLocale
  , parseLocale
  , TimeLocale(..)
  , LocaleParseException(..)
  ) where

import           Control.Applicative
import           Control.Exception
import           Data.Attoparsec.Text
import qualified Data.Text as Text
import           Data.Time.Format (TimeLocale(..))
import           Data.Typeable
import           System.Process

-- | Thrown when the locale cannot be parsed
data LocaleParseException =
  LocaleParseException String
  deriving (Show,Eq,Typeable)

instance Exception LocaleParseException

-- | 'Parser' for locales returned by the unix utility 'locale'
parseLocale :: Parser TimeLocale
parseLocale = do
  abDay <- parseSemicolonSeparatedLine
  day <- parseSemicolonSeparatedLine
  abMon <- parseSemicolonSeparatedLine
  mon <- parseSemicolonSeparatedLine
  [am, pm] <- parseSemicolonSeparatedLine
  dateTimeFmt' <- manyTill anyChar newline
  dateFmt' <- manyTill anyChar newline
  timeFmt' <- manyTill anyChar newline
  time12Fmt' <- manyTill anyChar newline
  pure
    (TimeLocale
       (zip day abDay)
       (zip mon abMon)
       (am, pm)
       dateTimeFmt'
       dateFmt'
       timeFmt'
       time12Fmt'
       [])

-- | Read a locale with 'LC_TIME' set according to the first argument.
--
-- Throws a 'LocaleParseException' if the output of calling 'locale'
-- cannot be parsed.
--
-- The 'knownTimeZones' field will always be empty.
--
-- > getLocale (Just "en_US.UTF-8")
getLocale :: Maybe String -> IO TimeLocale
getLocale localeName = do
  output <- readCreateProcess (getLocaleProcess localeName) ""
  case parseOnly (parseLocale <* endOfInput) (Text.pack output) of
    Left err -> throwIO (LocaleParseException err)
    Right locale -> pure locale

-- | Get the current locale of the process.
--
-- Throws a 'LocaleParseException' if the output of calling 'locale'
-- cannot be parsed.
--
-- The 'knownTimeZones' field will always be empty.
getCurrentLocale :: IO TimeLocale
getCurrentLocale = getLocale Nothing

getLocaleProcess :: Maybe String -> CreateProcess
getLocaleProcess localeName =
  (proc "locale"
        ["abday"
        ,"day"
        ,"abmon"
        ,"mon"
        ,"am_pm"
        ,"d_t_fmt"
        ,"d_fmt"
        ,"t_fmt"
        ,"t_fmt_ampm"]) {env = toLangEnv <$> localeName}

newline :: Parser Char
newline = char '\n'

parseSemicolonSeparatedLine :: Parser [String]
parseSemicolonSeparatedLine =
  sepBy (many (satisfy (not . finalChar))) (char ';') <* newline
  where
    finalChar c = c == ';' || c == '\n'

toLangEnv :: String -> [(String,String)]
toLangEnv s = [("LC_TIME",s)]
