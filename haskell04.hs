-- Prática 04 de Haskell
-- Nome: Guilherme Medeiros da Cunha

faixaIdoso :: Int -> String
faixaIdoso i
    | i < 60 = "ND"
    | i < 65 = "IDO64"
    | i < 70 = "IDO69"
    | i < 75 = "IDO74"
    | i < 80 = "IDO79"
    | otherwise = "IDO80"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos l = [(x, y, faixaIdoso y) | (x, y) <- l]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' l = map (\ (x, y) -> (x, y, faixaIdoso y)) l