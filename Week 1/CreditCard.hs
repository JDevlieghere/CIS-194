import Data.Char

-- Convert positive Integers to a list of digits
toDigits :: Integer -> [Integer]
toDigits x = if x <= 0 then [] else convert $ show x
    where convert = map (toInteger . digitToInt)

-- Convert positive Integers to a list of digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

-- Double every other number beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEven (reverse xs) False
    where doubleEven [] _ = []
          doubleEven (x:xs) even = if even
                                   then (2*x):doubleEven xs False
                                   else x:doubleEven xs True

-- Sum the digits of the list of integers
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ toSingleDigits xs
    where toSingleDigits = foldr ((++) . toDigits) []

-- Returns the checksum
checksum :: Integer -> Integer
checksum xs = (sumDigits . doubleEveryOther . toDigits) xs `rem` 10

-- Validate the checksum
validate :: Integer -> Bool
validate x = checksum x == 0