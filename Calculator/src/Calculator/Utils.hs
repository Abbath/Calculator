module Calculator.Utils (hashify) where

import Data.Text as T (Text, singleton)

hashify :: Text -> Text
hashify = (T.singleton '#' <>)
