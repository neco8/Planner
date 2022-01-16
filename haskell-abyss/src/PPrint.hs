{-# LANGUAGE OverloadedStrings #-}
module PPrint where

import           Control.Arrow ((&&&))
import           Data.List     (intercalate)
import           Data.Ratio    (denominator, numerator)
import           Data.String   (IsString (fromString))
import qualified Data.Text     as T (Text, intercalate, lines)
import           Data.Tree     (Tree (rootLabel, subForest))
import           Data.Vector   (Vector)

class PPrint a where
  pprint :: a -> T.Text

instance PPrint a => PPrint (Tree a) where
  pprint tree =
    T.intercalate "\n" (pprint (rootLabel tree) : (("  " <>) <$> concatMap (T.lines . pprint) (subForest tree)))

instance PPrint Float where
  pprint f =
    case (numerator &&& denominator) $ toRational f of
      (a, 1) -> fromString $ show a
      _      -> fromString $ show f

instance PPrint T.Text where
  pprint = id

instance PPrint s => PPrint (Vector s) where
  pprint ts = foldMap (<> "\n") $ pprint <$> ts

instance PPrint s => PPrint [s] where
  pprint ts = foldMap (<> "\n") $ pprint <$> ts

instance (PPrint a, PPrint b) => PPrint (Either a b) where
  pprint e = case e of
    Right a -> pprint a
    Left b  -> pprint b
