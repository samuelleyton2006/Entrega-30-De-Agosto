validateIter :: Integer -> Bool
validateIter number =
  let digits = reverse $ map (read . (:[])) $ show number
      sumOfDigits = helper digits False 0
  in sumOfDigits `mod` 10 == 0
  where
    helper [] _ acc = acc
    helper (x:xs) isSecond acc
      | isSecond  = helper xs False (acc + (if x > 4 then x * 2 - 9 else x * 2))
      | otherwise = helper xs True (acc + x)
