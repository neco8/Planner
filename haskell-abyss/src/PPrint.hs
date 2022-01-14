{-# LANGUAGE OverloadedStrings #-}
module PPrint where

import           Data.List (intercalate)
import qualified Data.Text as T (Text, intercalate, lines)
import           Data.Tree

class PPrint a where
  pprint :: a -> T.Text

instance PPrint a => PPrint (Tree a) where
  pprint tree =
    T.intercalate "\n" (pprint (rootLabel tree) : (("  " <>) <$> concatMap (T.lines . pprint) (subForest tree)))
