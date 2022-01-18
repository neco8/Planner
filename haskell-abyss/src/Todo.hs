{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Todo (Todo (..), todoParser, isDone, content, VsCodeTodo (..), IsTodo) where
import           Data.Functor         ((<$))
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import           Lens.Micro           ((^.))
import           Lens.Micro.TH        (makeLenses)
import           PPrint               (PPrint, pprint)
import           Parser               (Parser)
import           Text.Megaparsec      (choice, try, (<|>))
import           Text.Megaparsec.Char (char, string)

data Todo s = Todo
  { _isTodoDone :: Bool
  , _content    :: s
  } deriving (Eq, Ord, Show, Functor)
makeLenses ''Todo

class IsTodo t where
  isDone :: t -> Bool

instance IsTodo (Todo s) where
  isDone = (^. isTodoDone)

instance PPrint s => PPrint (Todo s) where
  pprint =
    showDoneWith $ \b -> "- [" <> (if b then "x" else " ") <> "] "

newtype VsCodeTodo s = VsCodeTodo (Todo s) deriving (Eq, Ord, Show)

instance IsTodo (VsCodeTodo s) where
  isDone (VsCodeTodo todo) = isDone todo

instance PPrint s => PPrint (VsCodeTodo s) where
  pprint (VsCodeTodo todo) =
    showDoneWith (\b -> if b then "✔ " else "☐ ") todo

showDoneWith :: PPrint s => (Bool -> Text) -> Todo s -> Text
showDoneWith f (Todo b s) =
  f b <> pprint s

instance (IsTodo a, IsTodo b) => IsTodo (Either a b) where
  isDone e = case e of
    Right a -> isDone a
    Left b  -> isDone b

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
