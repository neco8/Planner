{-# LANGUAGE OverloadedStrings #-}
module ForWork where

import qualified ActionPriorityMatrix as APM (ActionPriorityMatrix, Tag (..),
                                              name, qwas, tags)
import qualified Data.Text            as T (Text, intercalate, lines, pack,
                                            unpack)
import           Data.Tree            (Tree)
import           GHC.Exts             (IsString (fromString))
import           Lens.Micro           (to, (^.))
import           PPrint               (PPrint (..))
import           Parser               (Parser)
import qualified QuickWinAnalysis     as QWA (QuickWinAnalysis, name)
import           Text.Megaparsec      (anySingle, many, manyTill_, parseMaybe,
                                       satisfy, some)
import           Text.Megaparsec.Char (char)
import           Todo                 (Todo)

work :: APM.Tag
work = APM.Tag "work"

newtype ForWorkActionPriorityMatrix = ForWorkAPM (APM.ActionPriorityMatrix (Tree (Todo QWA.QuickWinAnalysis)))

instance PPrint ForWorkActionPriorityMatrix where
  pprint (ForWorkAPM apm)
    | work `notElem` apm ^. APM.tags = ""
    | otherwise = join "\n" ("- " <> apm ^. APM.name . to pprint) $ apm ^. APM.qwas . to (fmap (fmap (fmap ForWorkQWA))) . to pprint . to (indent "  ")
    where
      -- TODO: 後で共通化できる
      join joiner a b
        | a /= mempty && b /= mempty = a <> joiner <> b
        | otherwise = a <> b
      indent :: T.Text -> T.Text -> T.Text
      indent space txt =
        T.intercalate "\n" ((space <>) <$> T.lines txt)

newtype ForWorkQuickWinAnalysis = ForWorkQWA QWA.QuickWinAnalysis

instance PPrint ForWorkQuickWinAnalysis where
  pprint (ForWorkQWA qwa) =
    pprint $ qwa ^. QWA.name

changeFilePathForWork :: FilePath -> Maybe FilePath
changeFilePathForWork f = do
  (name, dotextension) <- parseMaybe parser $ fromString f
  pure . T.unpack $ name <> "_work" <> dotextension
  where
    parser :: Parser (T.Text, T.Text)
    parser = do
      (name, dotextension) <- manyTill_ anySingle $ do
        dot <- char '.'
        cs <- some $ satisfy (\s -> s /= ' ' && s /= '.')
        pure $ T.pack $ dot : cs
      pure (T.pack name, dotextension)
