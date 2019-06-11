module Main where

import           Control.Applicative
import           Data.Bits           (shift)
import           Data.Word           (Word32)
import           Text.Trifecta

newtype IPAddress =
  IPAddress
    { ip :: Word32
    }
  deriving (Eq, Ord, Show)

mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)

parseIPAddress :: Parser IPAddress
parseIPAddress = IPAddress <$> ipAddr
  where
    ipAddr :: Parser Word32
    ipAddr =
      foldr (\(s, v) acc -> shift v s + acc) 0 <$>
      (zip [24, 16, 8, 0] <$> octets)
    octet :: Parser [Char]
    octet = do
      xs <- some (oneOf "0123456789")
      case xs of
        ('0':_:_) -> fail "Octet can't have leading zero"
        _         -> return xs
    octets :: Parser [Word32]
    octets = do
      xs <- (fmap . fmap) read $ sepBy octet (char '.')
      case length xs of
        4 -> do
          case all (<= 255) xs of
            True -> return xs
            _    -> fail "IPAddress octets must be less than 256"
        _ -> fail "IPAddress must have 4 octets"

validOne = "192.168.0.1"
validTwo = "172.16.254.1"
validThree = "204.120.0.15"

invalidOne = "00.00.00.00"
invalidTwo = "010.010.010.010"

p :: Parser a -> String -> Result a
p f = parseString f mempty

main :: IO ()
main = do
  print $ p parseIPAddress validOne
  print $ p parseIPAddress validTwo
  print $ p parseIPAddress validThree
  print $ p parseIPAddress invalidOne
  print $ p parseIPAddress invalidTwo
