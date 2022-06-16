module Cupcake
    ( name, price, sugar, nuts, chocolate, cupcake, cookie
    ) where

type Cake = [String]

type Bundle = [Cake]

name :: Cake -> String
name [] = ""
name cake
    | length cake == 1 = addIcon (head cake)
    | length cake == 2 = createShortName cake
    | otherwise = addIcon (head cake) ++ " with " ++ createLongName (tail cake)

createLongName :: Cake -> String
createLongName [] = ""
createLongName (x:xs)
    | length (x:xs) == 1 = addIcon x
    | otherwise = addIcon x ++ " and " ++ createLongName xs


createShortName :: Cake -> String
createShortName [] = ""
createShortName (x:xs)
    | length (x:xs) == 1 = addIcon x
    | otherwise = addIcon x ++ " with " ++ createShortName xs

addIcon :: String -> String
addIcon icon
    | icon == "cookie" = "🍪"
    | icon == "cupcake" = "🧁"
    | icon == "sugar" = "🍬"
    | icon == "nuts" = "🥜"
    | icon == "chocolate" = "🍫"
    | otherwise = error "no icon available"

cupcake :: Cake
cupcake = ["cupcake"]

cookie :: Cake
cookie = ["cookie"]

sugar :: Cake -> Cake
sugar cake = cake ++ ["sugar"]

nuts :: Cake -> Cake
nuts cake = cake ++ ["nuts"]

chocolate :: Cake -> Cake
chocolate cake = cake ++ ["chocolate"]

price :: Cake -> String
price [] = "0.0$"
price ingredients = show (addPrice ingredients) ++ "$"

addPrice :: Cake -> Float
addPrice [] = 0
addPrice (x:xs)
    | x == "cookie" = 2 + addPrice xs
    | x == "cupcake" = 1 + addPrice xs
    | x == "sugar" = 0.3 + addPrice xs
    | x == "nuts" = 0.2 + addPrice xs
    | x == "chocolate" = 0.1 + addPrice xs
    | otherwise = error "no icon available"