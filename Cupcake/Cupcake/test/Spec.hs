import Cupcake (name, price, sugar, nuts, chocolate, cupcake, cookie)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Test name function" $ do
        it "test cupcake" $
            let myCake = ["cupcake"]
                actual = name myCake
                expected = "🧁"
            in actual `shouldBe` expected
        it "test cupcake with chocolate" $
            let myCake = ["cupcake", "chocolate"]
                actual = name myCake
                expected = "🧁 with 🍫"
            in actual `shouldBe` expected
        it "test cupcake with nuts" $
            let myCake = ["cupcake", "nuts"]
                actual = name myCake
                expected = "🧁 with 🥜"
            in actual `shouldBe` expected
        it "test cupcake with sugar" $
            let myCake = ["cupcake", "sugar"]
                actual = name myCake
                expected = "🧁 with 🍬"
            in actual `shouldBe` expected
        it "test cupcake with chocolate and nuts" $
            let myCake = ["cupcake", "chocolate", "nuts"]
                actual = name myCake
                expected = "🧁 with 🍫 and 🥜"
            in actual `shouldBe` expected
        it "test cupcake with nuts and chocolate" $
            let myCake = ["cupcake", "nuts", "chocolate"]
                actual = name myCake
                expected = "🧁 with 🥜 and 🍫"
            in actual `shouldBe` expected
        it "test cupcake with nuts, chocolate and sugar" $
            let myCake = ["cupcake", "nuts", "chocolate", "sugar"]
                actual = name myCake
                expected = "🧁 with 🥜 and 🍫 and 🍬"
            in actual `shouldBe` expected
        it "test cookie" $
            let myCake = ["cookie"]
                actual = name myCake
                expected = "🍪"
            in actual `shouldBe` expected
        it "test cookie with chocolate" $
            let myCake = ["cookie", "chocolate"]
                actual = name myCake
                expected = "🍪 with 🍫"
            in actual `shouldBe` expected
        it "test cookie with nuts" $
            let myCake = ["cookie", "nuts"]
                actual = name myCake
                expected = "🍪 with 🥜"
            in actual `shouldBe` expected
        it "test cookie with sugar" $
            let myCake = ["cookie", "sugar"]
                actual = name myCake
                expected = "🍪 with 🍬"
            in actual `shouldBe` expected
        it "test cookie with chocolate and nuts" $
            let myCake = ["cookie", "chocolate", "nuts"]
                actual = name myCake
                expected = "🍪 with 🍫 and 🥜"
            in actual `shouldBe` expected
        it "test cookie with nuts and chocolate" $
            let myCake = ["cookie", "nuts", "chocolate"]
                actual = name myCake
                expected = "🍪 with 🥜 and 🍫"
            in actual `shouldBe` expected
        it "test cookie with nuts, chocolate and nuts" $
            let myCake = ["cookie", "nuts", "chocolate", "sugar"]
                actual = name myCake
                expected = "🍪 with 🥜 and 🍫 and 🍬"
            in actual `shouldBe` expected
    describe "Test price function" $ do
        it "test cupcake" $
            let myCake = ["cupcake"]
                actual = price myCake
                expected = "1.0$"
            in actual `shouldBe` expected
        it "test cookie" $
            let myCake = ["cookie"]
                actual = price myCake
                expected = "2.0$"
            in actual `shouldBe` expected
        it "test cupcake with chocolate" $
            let myCake = ["cupcake", "chocolate"]
                actual = price myCake
                expected = "1.1$"
            in actual `shouldBe` expected
        it "test cookie with chocolate" $
            let myCake = ["cookie", "chocolate"]
                actual = price myCake
                expected = "2.1$"
            in actual `shouldBe` expected
        it "test cookie with nuts" $
            let myCake = ["cookie", "nuts"]
                actual = price myCake
                expected = "2.2$"
            in actual `shouldBe` expected
        it "test cupcake with sugar" $
            let myCake = ["cupcake", "sugar"]
                actual = price myCake
                expected = "1.3$"
            in actual `shouldBe` expected