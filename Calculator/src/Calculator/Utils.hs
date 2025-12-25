module Calculator.Utils (hashify, splitRational) where

import Data.Ratio (denominator, numerator)
import Data.Text as T (Text, singleton)

hashify :: Text -> Text
hashify = (T.singleton '#' <>)

splitRational :: Rational -> (Integer, Integer)
splitRational x = (numerator x, denominator x)
