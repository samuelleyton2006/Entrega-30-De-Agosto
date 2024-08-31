hanoiFoldr :: Integer -> [Movimiento]
hanoiFoldr n = foldr (\i acc -> acc ++ [Mover 1 3] ++ [Mover 2 1 | j <- [1..(2^i - 1)]]) [] [1..n]
