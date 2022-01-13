module PPrint where

class PPrint a where
  pprint :: a -> String