import           Control.DeepSeq
import           Control.Exception
import           Data.Ratio        ((%))
import           Debug.Trace
import           Lib
import           Test.Hspec
import           Test.HUnit.Lang   (assertEqual, assertFailure)
import           Text.Trifecta

instance Eq a => Eq (Result a) where
  Success x == Success y = x == y
  _ == _ = error "Failures are uncomparable"

instance NFData a => NFData (Result a) where
  rnf (Success x) = rnf x
  rnf (Failure e) = ()

shouldParse :: (Show a, Eq a) => a -> Result a -> Expectation
shouldParse x (Success x') = assertEqual "Parse succeeds" x x'
shouldParse _ (Failure _)  = assertFailure "Parse failure"

main :: IO ()
main =
  hspec $ do
    describe "parseRational" $ do
      it "should succeed on \"1/1\"" $ do
        shouldParse example $ p parseRational "1/1"
      it "should emit on invalid ratio \"1/0\"" $ do
        (evaluate . force) (p parseRational "1/0") `shouldThrow` anyException
      it "should emit on partial ratio \"1/\"" $ do
         shouldParse example (p parseRational "1/") `shouldThrow` anyException
      it "should emit on partial ratio \"/1\"" $ do
         shouldParse example (p parseRational "/1") `shouldThrow` anyException
    describe "parseRational'" $ do
      it "should succeed on \"1/1\"" $ do
        shouldParse example $ p parseRational' "1/1"
      it "should emit on invalid ratio \"1/0\"" $ do
        shouldParse example (p parseRational' "1/0") `shouldThrow` anyException
      it "should emit on partial ratio \"1/\"" $ do
         shouldParse example (p parseRational' "1/") `shouldThrow` anyException
      it "should emit on partial ratio \"/1\"" $ do
         shouldParse example (p parseRational' "/1") `shouldThrow` anyException
  where
    example = 1 % 1
