{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module NumbersInWords
    ( convertToWords, separateDigits
    ) where

import Data.Char

units = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
tens = ["", "ten", "twenty", "thirty", "fourty", "fivety", "sixty", "seventy", "eightty", "ninety"]

convertToWords :: String -> String
convertToWords = convert . separateDigits

separateDigits :: String -> [Int]
separateDigits [] = []
separateDigits (x:xs) = [(ord x) - 48] ++ separateDigits xs

convert :: [Int] -> String
convert [] = ""
convert (n:ns)
    | length (n:ns) == 1 = units !! n
    | length (n:ns) == 2 = tens !! n ++ (convert $ tail ns)
    | length (n:ns) == 3 = units !! n ++ "hundred" ++ (convert $ tail ns)
    | length (n:ns) == 4 = units !! n ++ "thousand" ++ (convert $ tail ns)
    | otherwise = error "to big number"
