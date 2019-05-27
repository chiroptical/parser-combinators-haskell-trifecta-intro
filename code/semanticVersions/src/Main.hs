module Main where

import           Control.Applicative
import           Data.Maybe          (fromMaybe)
import           Text.Trifecta

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show, Ord)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Prerelease = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer =
  SemVer
    { major      :: Major
    , minor      :: Minor
    , patch      :: Patch
    , prerelease :: Prerelease
    , metadata   :: Metadata
    }
  deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer <$> versionElement <* char '.' <*> versionElement <* char '.' <*>
  versionElement <*>
  prerelease <*>
  metadata
  where
    optionally p = fromMaybe [] <$> optional p
    prerelease = optionally $ char '-' *> (numberOrString `sepBy` (symbol "."))
    metadata = optionally $ char '+' *> (numberOrString `sepBy` (symbol "."))
    versionElement =
      fmap read $
      ((:) <$> oneOf "123456789" <*> many (oneOf "0123456789") <|>
       some (char '0'))
    numberOrString = (NOSI <$> integer) <|> (NOSS <$> some alphaNum)

one :: String
one = "1.0.0-gamma+002"

two :: String
two = "1.0.0-beta+oof.sha.41af286"

three :: String
three = "1.0.0-x.7.z.92"

p :: Parser a -> String -> Result a
p f = parseString f mempty

main :: IO ()
main = do
  print $ p parseSemVer one
  print $ p parseSemVer two
  print $ p parseSemVer three
