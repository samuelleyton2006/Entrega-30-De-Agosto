
hanoi :: Integer -> String -> String -> String -> IO ()
hanoi 0 _ _ _ = return ()
hanoi n origen auxiliar destino = do
    hanoi (n-1) origen destino auxiliar
    putStrLn $ "Se ha movido el disco " ++ show n ++ " desde la torre '" ++ origen ++ "' a la torre '" ++ destino ++ "'"
    hanoi (n-1) auxiliar origen destino

resolverHanoi :: Integer -> IO ()
resolverHanoi n = hanoi n "A" "B" "C"


main :: IO ()
main = do
    putStrLn "Ingrese el número de discos:"
    n <- readLn
    putStrLn $ "Pasos para resolver las Torres de Hanoi con " ++ show n ++ " discos:"
    resolverHanoi n
