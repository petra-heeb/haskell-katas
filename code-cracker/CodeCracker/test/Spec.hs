import CodeCracker (encrypt, decrypt)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Test single characters - encrypt" $ do
        it "test a -> !" $
            let character = "a"
                actual = encrypt character
                expected = "!"
            in actual `shouldBe` expected
        it "test o -> d" $
            let character = "o"
                actual = encrypt character
                expected = "d"
            in actual `shouldBe` expected
        it "test z -> o" $
            let character = "z"
                actual = encrypt character
                expected = "o"
            in actual `shouldBe` expected
    describe "Test multiple characters - encrypt" $ do
        it "test aa -> !!" $
            let string = "aa"
                actual = encrypt string
                expected = "!!"
            in actual `shouldBe` expected
        it "test hello -> &£aad" $
            let string = "hello"
                actual = encrypt string
                expected = "&£aad"
            in actual `shouldBe` expected
        it "test world -> ldga(" $
            let string = "world"
                actual = encrypt string
                expected = "ldga("
            in actual `shouldBe` expected
    describe "Test single characters - decrypt" $ do
        it "test ! -> a" $
            let character = "!"
                actual = decrypt character
                expected = "a"
            in actual `shouldBe` expected
        it "test d -> o" $
            let character = "d"
                actual = decrypt character
                expected = "o"
            in actual `shouldBe` expected
        it "test o -> z" $
            let character = "o"
                actual = decrypt character
                expected = "z"
            in actual `shouldBe` expected
    describe "Test multiple characters - decrypt" $ do
        it "test !! -> aa" $
            let string = "!!"
                actual = decrypt string
                expected = "aa"
            in actual `shouldBe` expected
        it "test &£aad -> hello" $
            let string = "&£aad"
                actual = decrypt string
                expected = "hello"
            in actual `shouldBe` expected
        it "test ldga( -> world" $
            let string = "ldga("
                actual = decrypt string
                expected = "world"
            in actual `shouldBe` expected
    describe "Test both" $ do
        it "test enc + dec" $
            let string = "hello"
                actual = decrypt $ encrypt "hello"
                expected = "hello"
            in actual `shouldBe` expected