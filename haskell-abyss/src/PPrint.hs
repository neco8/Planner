module PPrint where

import           Data.List (intercalate)
import           Data.Tree

class PPrint a where
  pprint :: a -> String

instance PPrint a => PPrint (Tree a) where
  pprint tree =
    intercalate "\n" $ pprint (rootLabel tree) : (("  " <>) <$> concatMap (lines . pprint) (subForest tree))
