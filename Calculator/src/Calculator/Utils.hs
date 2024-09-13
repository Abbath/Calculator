module Calculator.Utils where

import Data.Text as T (Text, singleton)

attify :: Text -> Text
attify = (T.singleton '@' <>)