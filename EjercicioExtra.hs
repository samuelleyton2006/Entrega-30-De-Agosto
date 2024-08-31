-- Definición del tipo Movimiento
data Movimiento = Mover String String deriving (Show)

-- Función principal para resolver las Torres de Hanoi con cuatro estacas
hanoi4 :: Integer -> String -> String -> String -> String -> [Movimiento]
hanoi4 n origen destino auxiliar1 auxiliar2
    | n == 0    = []
    | n == 1    = [Mover origen destino]
    | otherwise = hanoi4 k origen auxiliar1 destino auxiliar2 ++
                  hanoi3 (n - k) origen destino auxiliar2 ++
                  hanoi4 k auxiliar1 destino origen auxiliar2
  where
    k = n - round (sqrt (2 * fromIntegral n + 1)) + 1 -- Valor óptimo de k

-- Función auxiliar para resolver el problema clásico de Hanoi con tres estacas
hanoi3 :: Integer -> String -> String -> String -> [Movimiento]
hanoi3 0 _ _ _ = []
hanoi3 n origen destino auxiliar = 
    hanoi3 (n-1) origen auxiliar destino ++
    [Mover origen destino] ++
    hanoi3 (n-1) auxiliar destino origen

-- Función para iniciar el proceso con los nuevos mensajes y cuatro estacas
resolverHanoi4 :: Integer -> IO ()
resolverHanoi4 n = do
    let movimientos = hanoi4 n "A" "B" "C" "D"
    mapM_ printMovimiento movimientos
  where
    printMovimiento (Mover desde hacia) = 
        putStrLn $ "¡Atención! Moviendo el disco desde la torre '" ++ desde ++ "' a la torre '" ++ hacia ++ "'"

-- Función principal
main :: IO ()
main = do
    putStrLn "Ingrese el número de discos:"
    n <- readLn
    putStrLn $ "Pasos para resolver las Torres de Hanoi con " ++ show n ++ " discos usando cuatro estacas:"
    resolverHanoi4 n
