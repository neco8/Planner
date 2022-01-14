module PPrint where

import Data.Tree
import Data.List (intercalate)

class PPrint a where
  pprint :: a -> String

instance PPrint a => PPrint (Tree a) where
  pprint tree =
    intercalate "\n" $ pprint (rootLabel tree) : (("  " <>) <$> concatMap (lines . pprint) (subForest tree))