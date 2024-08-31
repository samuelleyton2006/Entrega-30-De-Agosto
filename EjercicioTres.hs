sumDigitsMap :: Integer -> Integer
sumDigitsMap n = sum $ map (read . (:[])) (show n)
