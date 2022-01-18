{-# LANGUAGE OverloadedStrings #-}
module ForWork where

import qualified ActionPriorityMatrix as APM
import qualified Data.Text            as T
import           Data.Tree
import qualified Data.Vector          as V
import           Lens.Micro           (to, (^.))
import           PPrint
import qualified QuickWinAnalysis     as QWA
import           Todo

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
