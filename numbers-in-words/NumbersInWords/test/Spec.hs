import NumbersInWords
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe " Test separateDigits function" $ do
        it "test 1234 -> [1,2,3,4]" $
            let input = "1234"
                actual = separateDigits input
                expected = [1,2,3,4]
            in actual `shouldBe` expected
    describe "Test units" $ do
        it "test 1" $
            let input = "1"
                actual = convertToWords input
                expected = "one"
            in actual `shouldBe` expected
        it "test 5" $
            let input = "5"
                actual = convertToWords input
                expected = "five"
            in actual `shouldBe` expected
        it "test 9" $
            let input = "9"
                actual = convertToWords input
                expected = "nine"
            in actual `shouldBe` expected
    describe "Test tens" $ do
        it "test 10" $
            let input = "10"
                actual = convertToWords input
                expected = "ten"
            in actual `shouldBe` expected
        it "test 50" $
            let input = "50"
                actual = convertToWords input
                expected = "fivety"
            in actual `shouldBe` expected
        it "test 90" $
            let input = "90"
                actual = convertToWords input
                expected = "ninety"
            in actual `shouldBe` expected
    describe "Test hundreds" $ do
        it "test 100" $
            let input = "100"
                actual = convertToWords input
                expected = "onehundred"
            in actual `shouldBe` expected
        it "test 500" $
            let input = "500"
                actual = convertToWords input
                expected = "fivehundred"
            in actual `shouldBe` expected
        it "test 900" $
            let input = "900"
                actual = convertToWords input
                expected = "ninehundred"
            in actual `shouldBe` expected
    describe "Test tousands" $ do
        it "test 1000" $
            let input = "1000"
                actual = convertToWords input
                expected = "onethousand"
            in actual `shouldBe` expected
        it "test 5000" $
            let input = "5000"
                actual = convertToWords input
                expected = "fivethousand"
            in actual `shouldBe` expected
        it "test 9000" $
            let input = "9000"
                actual = convertToWords input
                expected = "ninethousand"
            in actual `shouldBe` expected