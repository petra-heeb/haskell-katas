module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateQuality :: GildedRose -> GildedRose
updateQuality (Item name sellIn quality) = (Item name (changeSellIn name sellIn) (changeQuality name sellIn quality))
{-updateQuality = map updateQualityItem
  where
    updateQualityItem (Item name sellIn quality) =
      let
        quality' = changeQuality name sellIn quality
        sellIn' = changeSellIn name sellIn
      in
        if sellIn' < 0
        then
          if name /= "Aged Brie"
          then
            if name /= "Backstage passes to a TAFKAL80ETC concert"
            then
              if quality' > 0
              then
                if name /= "Sulfuras, Hand of Ragnaros"
                then (Item name sellIn' (quality' - 1))
                else (Item name sellIn' quality')
              else (Item name sellIn' quality')
            else (Item name sellIn' (quality' - quality'))
          else
            if quality' < 50
            then (Item name sellIn' (quality' + 1))
            else (Item name sellIn' quality')
        else (Item name sellIn' quality')
-}
updateQualityItem :: Item -> Item
updateQualityItem (Item name sellIn quality) = (Item name (changeSellIn name sellIn) (changeQuality name sellIn quality)) 

changeSellIn :: String -> Int -> Int -> Int
changeSellIn name sellIn quality
  | sellIn >= 0 = Item name sellIn quality
  | name == "Aged Brie" = updateAgedBrieQuality quality
  | name == "Backstage passes to a TAFKAL80ETC concert" = Item name sellIn 0
  | quality <= 0 = Item name sellIn quality
  | name == "Sulfuras, Hand of Ragnaros" = Item name sellIn quality
  | otherwise = Item name sellIn $ updateNormalQuality quality

decreaseSellIn :: GildedRose -> GildedRose
decreaseSellIn = undefined
if sellIn' < 0
        then
          if name /= "Aged Brie"
          then
            if name /= "Backstage passes to a TAFKAL80ETC concert"
            then
              if quality' > 0
              then
                if name /= "Sulfuras, Hand of Ragnaros"
                then (Item name sellIn' (quality' - 1))
                else (Item name sellIn' quality')
              else (Item name sellIn' quality')
            else (Item name sellIn' (quality' - quality'))
          else
            if quality' < 50
            then (Item name sellIn' (quality' + 1))
            else (Item name sellIn' quality')
        else (Item name sellIn' quality')
  | name == "Backstage passes to a TAFKAL80ETC concert" = updateBackstagePassesQuality sellIn quality
  | name == "Sulfuras, Hand of Ragnaros" = updateSulfurasQuality quality
  | name == "Conjured" = updateConjuredQuality quality
  | otherwise = updateNormalQuality quality
                
updateAgedBrieQuality :: Int -> Int
updateAgedBrieQuality quality
  | quality >= 50 = quality
  | otherwise = increaseQuality quality 1

updateSulfurasQuality :: Int -> Int
updateSulfurasQuality quality = 80

updateBackstagePassesQuality :: Int -> Int -> Int
updateBackstagePassesQuality sellIn quality
  | quality == 50 = quality
  | sellIn > 10 = increaseQuality quality 1
  | sellIn > 5 = increaseQuality quality 2
  | sellIn > 0 = increaseQuality quality 3
  | otherwise = 0

updateConjuredQuality :: Int -> Int
updateConjuredQuality quality 
  | quality > 0 = decreaseQuality quality 2
  | otherwise = quality

updateNormalQuality :: Int -> Int
updateNormalQuality quality 
  | quality > 0 = decreaseQuality quality 1
  | otherwise = quality

increaseQuality :: Int -> Int -> Int
increaseQuality quality amount = quality + amount

decreaseQuality :: Int -> Int -> Int
decreaseQuality quality amount = quality - amount

changeSellIn :: String -> Int -> Int
changeSellIn name sellIn
  | name == "Sulfuras, Hand of Ragnaros" = sellIn
  | otherwise = sellIn - 1