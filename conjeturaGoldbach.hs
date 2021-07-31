menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimoInt :: Integer -> Integer
esPrimoInt n | n == 1 = 0
             | n == menorDivisor n = n
             | otherwise = 0

esPrimoBool :: Integer -> Bool
esPrimoBool 1 = False
esPrimoBool n = (n == menorDivisor n)


-- n - k = primo => vale la conjetura k = numero para verificar la conjetura k = primo


numeroDeDescomposicionesAux :: Integer -> Integer -> Integer
numeroDeDescomposicionesAux k n | k == n = 0
                                | esPrimoBool t = 1 + numeroDeDescomposicionesAux (k+1) n
                                | otherwise = numeroDeDescomposicionesAux (k+1) n
                                  where t = n-esPrimoInt k

numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n = numeroDeDescomposicionesAux 2 n 

satisfaceGoldbachAux :: Integer -> Integer -> Bool
satisfaceGoldbachAux k n | n == k = False
                         | esPrimoBool t = True
                         | otherwise = satisfaceGoldbachAux (k+1) n
                           where t = n-esPrimoInt k

satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n | n `mod` 2 == 0 && n > 2 = satisfaceGoldbachAux 2 n
                    | otherwise = False

descomposicionEnPrimosAux :: Integer -> Integer -> (Integer,Integer)
descomposicionEnPrimosAux k n | n == k = (0,0)
                              | esPrimoBool t = (esPrimoInt k,t)
                              | otherwise = descomposicionEnPrimosAux (k+1) n
                                where t = n-esPrimoInt k

descomposicionEnPrimos :: Integer -> (Integer,Integer)
descomposicionEnPrimos n = descomposicionEnPrimosAux 2 n

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n | n == 2 = True
                          | satisfaceGoldbach n = satisfaceGoldbach (n-2)
                          | otherwise = False