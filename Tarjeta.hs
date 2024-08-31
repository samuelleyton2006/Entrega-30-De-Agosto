traceSteps :: [Int] -> [Int] -> [Int] -> Int -> Bool -> Bool
traceSteps digits doubled summed total isValid = do
    putStrLn $ "Dígitos del número: " ++ show digits
    putStrLn $ "Dígitos después de duplicar cada segundo desde la derecha: " ++ show doubled
    putStrLn $ "Suma de los dígitos individuales: " ++ show summed
    putStrLn $ "Suma total de los dígitos: " ++ show total
    if isValid
        then putStrLn "Resultado: El número es válido según el algoritmo de Luhn."
        else putStrLn "Resultado: El número no es válido según el algoritmo de Luhn."
    isValid
