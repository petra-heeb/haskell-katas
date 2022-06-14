import RSA (encrypt, decrypt, computeBigN, computeSmallN, chooseC)
import Test.Hspec
import Test.QuickCheck

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "Test computeBigN function" $ do
        it "Test p=51581 q=60101" $
            let p = 51581
                q = 60101
                actual = computeBigN p q
                expected = 3100069681
            in actual `shouldBe` expected
    describe "Test computeSmallN function" $ do
        it "Test p=51581 q=60101" $
            let p = 51581
                q = 60101
                actual = computeSmallN p q
                expected = 3099958000
            in actual `shouldBe` expected
    {-describe "Test chooseC function" $ do
        it "Test p=51581 q=60101" $
            let p = 51581
                q = 60101
                actual = chooseC p q
                n = computeSmallN p q
            in actual > 1 && actual < n`shouldBe` True-} -- untestable because of IO
    {-describe "Test encrypt character" $ do
        it "test" $
            let string = "a"
                actual = encrypt string
                expected = "n"
            in actual `shouldBe` expected-}