{-# LANGUAGE OverloadedStrings #-}
module ForWork where

import qualified ActionPriorityMatrix  as APM (ActionPriorityMatrix, Tag (..),
                                               name, qwas, tags)
import           AdditionalInformation (AdditionalInformation (AdditionalInformation),
                                        additionalInformationParser,
                                        runAdditionalInformation)
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T (Text, intercalate, lines, pack,
                                             unpack)
import           Data.Tree             (Tree)
import           GHC.Exts              (IsString (fromString))
import           Lens.Micro            (to, (^.))
import           PPrint                (PPrint (..))
import           Parser                (Parser)
import qualified QuickWinAnalysis      as QWA (QuickWinAnalysis, name)
import           Replace.Megaparsec    (streamEdit)
import           Text.Megaparsec       (anySingle, many, manyTill_, parseMaybe,
                                        satisfy, some)
import           Text.Megaparsec.Char  (char)
import           Todo                  (Todo, doneAt, timeParser)

work :: APM.Tag
work = APM.Tag $ AdditionalInformation "work"

newtype ForWorkActionPriorityMatrix = ForWorkAPM (APM.ActionPriorityMatrix (Tree (Todo QWA.QuickWinAnalysis)))

instance PPrint ForWorkActionPriorityMatrix where
  pprint (ForWorkAPM apm)
    | work `notElem` apm ^. APM.tags = ""
    | otherwise = join "\n" ("- " <> apm ^. APM.name . to pprint) $ apm ^. APM.qwas . to (fmap (fmap ForWorkTodoQWA)) . to pprint . to (indent "  ")
    where
      join joiner a b
        | a /= mempty && b /= mempty = a <> joiner <> b
        | otherwise = a <> b
      indent :: T.Text -> T.Text -> T.Text
      indent space txt =
        T.intercalate "\n" ((space <>) <$> T.lines txt)

newtype ForWorkTodoQuickWinAnalysis = ForWorkTodoQWA (Todo QWA.QuickWinAnalysis)

instance PPrint ForWorkTodoQuickWinAnalysis where
  pprint (ForWorkTodoQWA todo) =
    streamEdit (do
      a <- runAdditionalInformation <$> additionalInformationParser
      case parseMaybe timeParser a of
        Just t  -> pure t
        Nothing -> fail "this is not at."
    ) (const "") . pprint $ fmap ForWorkQWA todo

newtype ForWorkQuickWinAnalysis = ForWorkQWA QWA.QuickWinAnalysis

instance PPrint ForWorkQuickWinAnalysis where
  pprint (ForWorkQWA qwa) =
    pprint $ qwa ^. QWA.name

type Suffix = T.Text

changeFilePathForWork :: Maybe Suffix -> FilePath -> Maybe FilePath
changeFilePathForWork suffix f = do
  (name, dotextension) <- parseMaybe parser $ fromString f
  pure . T.unpack $ name <> fromMaybe "_work" suffix <> dotextension
  where
    parser :: Parser (T.Text, T.Text)
    parser = do
      (name, dotextension) <- manyTill_ anySingle $ do
        dot <- char '.'
        cs <- some $ satisfy (\s -> s /= ' ' && s /= '.')
        pure $ T.pack $ dot : cs
      pure (T.pack name, dotextension)
