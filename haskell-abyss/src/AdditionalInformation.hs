{-# LANGUAGE OverloadedStrings #-}
module AdditionalInformation where

import           Control.Applicative  (Alternative (many))
import           Data.Coerce          (coerce)
import           Data.Text            (Text)
import           Parser               (Parser)
import           Text.Megaparsec      (MonadParsec (takeWhile1P))
import           Text.Megaparsec.Char (string)

newtype AdditionalInformation = AdditionalInformation Text deriving (Eq, Ord, Show)

runAdditionalInformation :: AdditionalInformation -> Text
runAdditionalInformation = coerce

addInformationTo :: Parser a -> Parser (a, [AdditionalInformation])
addInformationTo p = do
  a <- p
  information <- many additionalInformationParser
  pure (a, information)


additionalInformationParser :: Parser AdditionalInformation
additionalInformationParser = do
  string " #"
  AdditionalInformation <$> takeWhile1P Nothing (\a -> a /= ' ' && a /= '\n' && a /= '#')
