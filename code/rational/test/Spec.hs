import           Control.DeepSeq
import           Control.Exception
import           Data.Ratio        ((%))
import           Lib
import           Test.Hspec
import           Text.Trifecta

-- Don't understand how to deal with Failure
-- from Trifecta correctly. Our parseToEither
-- function is a hack. The `Eq (Result a)`
-- instance is also incomplete.
parseToEither :: Result a -> Either String a
parseToEither (Success x) = Right x
parseToEither (Failure e) = Left "Failure"

instance Eq a => Eq (Result a) where
  Success x == Success y = x == y

instance NFData a => NFData (Result a) where
  rnf (Success x) = rnf x

main :: IO ()
main =
  hspec $ do
    describe "parseRational" $ do
      it "should return Success 1 % 1" $ do
        p parseRational "1/1" `shouldBe` Success (1 % 1)
      it "should emit 'Ratio has zero denominator' exception" $ do
        (evaluate . force) (p parseRational "1/0") `shouldThrow` anyException
    describe "parseRational'" $ do
      it "should return Success 1 % 1" $ do
        p parseRational' "1/1" `shouldBe` Success (1 % 1)
      it "should return Left \"Failure\"" $ do
        parseToEither (p parseRational' "1/0") `shouldBe` Left "Failure"
