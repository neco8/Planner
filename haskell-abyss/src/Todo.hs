{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Todo (Todo (..), todoParser) where
import           Data.Functor         ((<$))
import           Data.Vector          (Vector)
import           PPrint               (PPrint, pprint)
import           Parser               (Parser)
import           Text.Megaparsec      (choice, try, (<|>))
import           Text.Megaparsec.Char (char, string)

data Todo s = Todo Bool s deriving (Eq, Ord, Show)

instance PPrint s => PPrint (Todo s) where
  pprint =
    showDoneWith $ \b -> "- [" <> (if b then "x" else " ") <> "] "
    where
      showDoneWith f (Todo b s) =
        f b <> pprint s

-- parser

todoParser_ :: Vector (Parser Bool) -> Parser s -> Parser (Todo s)
todoParser_ pdones ps = do
  b <- try (choice pdones) <|> pure False
  Todo b <$> ps

doneMDParser :: Parser Bool
doneMDParser = do
  string "- ["
  b <- (True <$ char 'x') <|> (False <$ char ' ')
  string "] "
  pure b

doneVsCodeParser :: Parser Bool
doneVsCodeParser = try (True <$ string "✔ ") <|> (False <$ string "☐ ")

todoParser :: Parser s -> Parser (Todo s)
todoParser = todoParser_ [doneMDParser, doneVsCodeParser]
