{-# LANGUAGE OverloadedStrings #-}
module PPrint where

import           Data.List (intercalate)
import qualified Data.Text as T (Text, lines, intercalate)
import           Data.Tree
import           Prelude   hiding (lines, intercalate)

class PPrint a where
  pprint :: a -> T.Text

instance PPrint a => PPrint (Tree a) where
  pprint tree =
    T.intercalate "\n" (pprint (rootLabel tree) : (("  " <>) <$> concatMap (T.lines . pprint) (subForest tree)))
