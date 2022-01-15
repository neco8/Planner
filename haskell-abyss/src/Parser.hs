{-# LANGUAGE OverloadedStrings #-}
module Parser where
import           Data.Functor               (void)
import           Data.Functor.Identity      (Identity)
import           Data.Text                  (Text)
import           Data.Tree                  (Tree (..))
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParsecT, empty, some, (<|>))
import           Text.Megaparsec.Char       (char, space1, tab)
import qualified Text.Megaparsec.Char.Lexer as L (IndentOpt (..), float,
                                                  indentBlock, space)

type Parser = ParsecT Void Text Identity

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> tab)) empty empty

parserFromMaybe :: String -> a -> (a -> Maybe b) -> Parser b
parserFromMaybe errMsg a f = case f a of
  Just b  -> pure b
  Nothing -> fail errMsg

treeParser :: Parser s -> Parser (Tree s)
treeParser p = L.indentBlock scn $ do
  s <- p
  pure $ L.IndentMany Nothing (pure . Node s) $ treeParser p
