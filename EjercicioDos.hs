doubleEveryOtherAcc :: [Integer] -> [Integer]
doubleEveryOtherAcc xs = helper xs False
  where
    helper [] _ = []
    helper (y:ys) isSecond
        | isSecond  = (2 * y) : helper ys False
        | otherwise = y : helper ys True

everyOtherAcc :: [a] -> [a]
everyOtherAcc xs = helper xs False
  where
    helper [] _ = []
    helper (y:ys) isSecond
        | isSecond  = helper ys False
        | otherwise = y : helper ys True
