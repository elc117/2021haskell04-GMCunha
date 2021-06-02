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

strColor :: (Int,Int,Int) -> String
strColor (x, y, z) = "rgb(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n t r = [(x, snd t, r) | x <- [fst t + 0*r, fst t + 2*r .. fst t + 2*(n-1)*r]]

-- nessa optei por mostrar tons de vermelho entre 80 e 255 com intervalos calculados baseado no numero de tons desejado
genReds :: Int -> [(Int,Int,Int)]
genReds n = [(x , 0, 0) | x <- [80 + (quot 175 n)*0, 80 + (quot 175 n)*1 .. 80 + 175]]