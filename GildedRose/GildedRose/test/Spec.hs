import GuildedRose
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "update quality for Normal Items" $ do
        it "update quality which is 2 before" $
            let inventory = [Item "foo" 2 2]
                actual = updateQuality inventory
                expected = [Item "foo" 1 1]
            in actual `shouldBe` expected
        it "update quality which is 0 before" $
            let inventory = [Item "foo" 0 0]
                actual = updateQuality inventory
                expected = [Item "foo" (-1) 0]
            in actual `shouldBe` expected
    describe "Test updateQuality for Aged Brie" $ do
        it "update quality when aged 1 day" $
            let inventory = [Item "Aged Brie" 2 5]
                actual = updateQuality inventory
                expected = [Item "Aged Brie" 1 6]
            in actual `shouldBe` expected
        it "update quality when quality is max." $ 
            let inventory = [Item "Aged Brie" 4 50]
                actual = updateQuality inventory
                expected = [Item "Aged Brie" 3 50]
            in actual `shouldBe` expected
    describe "Test updateQuality for Backstage Passes" $ do
        it "update quality when aged 1 day over 10 days to sell date" $
            let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 20 5]
                actual = updateQuality inventory
                expected = [Item "Backstage passes to a TAFKAL80ETC concert" 19 6]
            in actual `shouldBe` expected
        it "update quality when aged 1 day under 10 days to sell date" $ 
            let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 7 5]
                actual = updateQuality inventory
                expected = [Item "Backstage passes to a TAFKAL80ETC concert" 6 7]
            in actual `shouldBe` expected
        it "update quality when aged 1 day under 5 days to sell date" $
            let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 2 5]
                actual = updateQuality inventory
                expected = [Item "Backstage passes to a TAFKAL80ETC concert" 1 8]
            in actual `shouldBe` expected
        it "update quality when aged 1 day after concert" $ 
            let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" (-1) 5]
                actual = updateQuality inventory
                expected = [Item "Backstage passes to a TAFKAL80ETC concert" (-2) 0]
            in actual `shouldBe` expected
    describe "Test updateQuality for Sulfuras" $ do
        it "update quality when aged 1 day" $
            let inventory = [Item "Sulfuras, Hand of Ragnaros" 2 80]
                actual = updateQuality inventory
                expected = [Item "Sulfuras, Hand of Ragnaros" 2 80]
            in actual `shouldBe` expected
    describe "update quality for Conjured Items" $ do
        it "update quality which is 2 before" $
            let inventory = [Item "Conjured" 2 6]
                actual = updateQuality inventory
                expected = [Item "Conjured" 1 4]
            in actual `shouldBe` expected
        it "update quality which is 0 before" $
            let inventory = [Item "Conjured" 0 0]
                actual = updateQuality inventory
                expected = [Item "Conjured" (-1) 0]
            in actual `shouldBe` expected
