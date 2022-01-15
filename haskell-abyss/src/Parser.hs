{-# LANGUAGE OverloadedStrings #-}
module Parser where
import           Control.Arrow              ((&&&))
import           Data.Functor               (void)
import           Data.Functor.Identity      (Identity)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import           Data.Tree                  (Tree (..))
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParsecT, empty, satisfy, some,
                                             (<|>))
import           Text.Megaparsec.Char       (char, hspace, space, space1, tab)
import qualified Text.Megaparsec.Char.Lexer as L (IndentOpt (..), float,
                                                  indentBlock, space)

type Parser = ParsecT Void Text Identity

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> tab)) empty empty

parserFromMaybe :: String -> Parser (Maybe a) -> Parser a
parserFromMaybe errMsg pma = do
  ma <- pma
  case ma of
    Just a  -> pure a
    Nothing -> fail errMsg

treeParser :: Parser s -> Parser (Tree s)
treeParser p = L.indentBlock scn $ do
  s <- p
  pure $ L.IndentMany Nothing (pure . Node s) $ treeParser p

separatedParser :: Parser Text
separatedParser = fromString <$> some (satisfy (uncurry (&&) . ((/= '\n') &&& (/= ','))))

symbol :: Parser a -> Parser a
symbol p = do
  space
  a <- p
  space
  pure a

hsymbol :: Parser a -> Parser a
hsymbol p = do
  hspace
  a <- p
  hspace
  pure a
