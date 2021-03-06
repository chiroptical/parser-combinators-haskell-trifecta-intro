module Lib where

import           Data.Ratio    ((%))
import           Text.Trifecta

p :: Parser a -> String -> Result a
p f i = parseString f mempty i

parseRational :: Parser Rational
parseRational = (%) <$> numer <*> (char '/' *> denom)
  where
    numer = decimal
    denom = decimal

parseRational' :: Parser Rational
parseRational' = (%) <$> numer <*> (char '/' *> denom)
  where
    numer = decimal
    denom = do
      d <- decimal
      case d of
        0 -> fail "denominator cannot be zero"
        _ -> return d
