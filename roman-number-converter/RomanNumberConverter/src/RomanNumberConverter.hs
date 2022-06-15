module RomanNumberConverter
    ( convertToRoman, replace
    ) where

units = ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
tens = ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"] 
hunderts = ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
thousands = ["", "M", "MM", "MMM"]

convertToRoman :: Int -> String
convertToRoman decimal = replace $ toDigits decimal

toDigits :: Int -> [Int]
toDigits n
    | n >=0 && n < 10 = [n]
    | n >= 10 = toDigits (n`div`10) ++ [n`mod`10]
    | otherwise = error "make sure your input is greater than 0" 

replace :: [Int] -> String
replace [] = ""
replace digits 
    | length digits == 1 = units !! (head digits)
    | length digits == 2 = tens !! (head digits) ++ replace  (tail digits)
    | length digits == 3 = hunderts !! (head digits) ++ replace (tail digits)
    | length digits == 4 = thousands !! (head digits) ++ replace (tail digits)