module Calculator.Utils (hashify, splitRational, isWhole) where

import Data.Complex (Complex (..))
import Data.Ratio (denominator, numerator)
import Data.Text as T (Text, singleton)

hashify :: Text -> Text
hashify = (T.singleton '#' <>)

splitRational :: Rational -> (Integer, Integer)
splitRational x = (numerator x, denominator x)

isWhole :: Complex Rational -> Bool
isWhole (re :+ im) = im == 0 && denominator re == 1
