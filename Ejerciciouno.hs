toDigitsAcc :: Integer -> [Integer]
toDigitsAcc n = toDigitsHelper n []
  where
    toDigitsHelper 0 acc = acc
    toDigitsHelper m acc = toDigitsHelper (m `div` 10) ((m `mod` 10) : acc)

toDigitsRevAcc :: Integer -> [Integer]
toDigitsRevAcc n = toDigitsHelper n []
  where
    toDigitsHelper 0 acc = acc
    toDigitsHelper m acc = toDigitsHelper (m `div` 10) (acc ++ [m `mod` 10])
