module Todo (Todo (..)) where

import           Data.String (IsString)
import           PPrint      (PPrint, pprint)

data Todo s = Todo Bool s deriving (Eq, Ord, Show)

instance PPrint s => PPrint (Todo s) where
  pprint =
    showDoneWith $ \b -> "- [" <> (if b then "x" else " ") <> "] "
    where
      showDoneWith f (Todo b s) =
        f b <> pprint s
