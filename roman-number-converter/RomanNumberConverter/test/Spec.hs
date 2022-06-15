import RomanNumberConverter (convertToRoman, replace)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Test replace function" $ do
        it "test 1 -> I" $
            let decimalString = [1]
                actual = replace decimalString
                expected = "I"
            in actual `shouldBe` expected
    describe "Test single digits" $ do
        it "test 1 -> I" $
            let decimalString = 1
                actual = convertToRoman decimalString
                expected = "I"
            in actual `shouldBe` expected
        it "test 4 -> IV" $
            let decimalString = 4
                actual = convertToRoman decimalString
                expected = "IV"
            in actual `shouldBe` expected        
        it "test 10 -> X" $
            let decimalString = 10
                actual = convertToRoman decimalString
                expected = "X"
            in actual `shouldBe` expected
    describe "Test multiple digits" $ do
        it "test 25 -> XXV" $
            let decimalString = 25
                actual = convertToRoman decimalString
                expected = "XXV"
            in actual `shouldBe` expected