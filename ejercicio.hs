-- Punto 1

toDigitsRev :: Int -> [Int]
toDigitsRev 0 = []
toDigitsRev x = [mod x 10 | x > 0] ++ toDigitsRev (div x 10) 

toDigits :: Int -> [Int]
toDigits x = reverse (toDigitsRev x)

-- Punto 2

intListLength :: [Int] -> Int   
intListLength [] = 0                                                
intListLength (x:xs) = 1 + intListLength xs

doubleEveryOther :: [Int]->[Int]
doubleEveryOther [] = []
doubleEveryOther n
    |(intListLength n `mod` 2) == 0 = pair n
    |otherwise                      = nonpair n

pair :: [Int]->[Int]
pair [] = []
pair (x:xs:r) = x * 2 : xs : doubleEveryOther r

nonpair :: [Int]->[Int]
nonpair (x:[]) = x:[]
nonpair (x:xs:r) = x: xs * 2 : doubleEveryOther r

-- Punto 3

sumDigits :: [Int]->Int
sumDigits []    = 0
sumDigits (x:r) = sumNumber x + sumDigits r

sumNumber :: Int->Int
sumNumber n
    |n<10   =n
    |otherwise  = n `div` 10 + n `mod` 10

-- punto 4

validate :: Int -> Bool
validate n
	| sumDigits(doubleEveryOther (toDigits n)) `mod` 10 == 0 = True
	| otherwise = False

