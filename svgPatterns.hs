import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) só com tons de verde "hard-coded" 
-- (pode ser melhorado substituindo os valores literais por parâmetros)
-- Além disso, o que acontecerá se n for muito grande ou negativo?
greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

-- minhas funcoes
genReds :: Int -> [(Int,Int,Int)]
genReds n = take n [(x, 0, 0) | x <- [255, 255 - (ceiling (175.0/fromIntegral n)) .. 40]]

genGreen :: Int -> [(Int,Int,Int)]
genGreen n = take n [(0, x, 0) | x <- [255, 255 - (ceiling (175.0/fromIntegral n)) .. 40]]

genBlue :: Int -> [(Int,Int,Int)]
genBlue n = take n [(0, 0, x) | x <- [255, 255 - (ceiling (175.0/fromIntegral n)) .. 40]]

rgbPalette' :: Int -> Int -> [(Int,Int,Int)]
rgbPalette' n p = take (n*p) $ cycle $ (genBlue p) ++ (genGreen p) ++ (genReds p) ++ (genGreen p)

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]



-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> Int -> [Rect]
genRectsInLine n p = [((m*w, o*h), w, h) | m <- [0..fromIntegral (n-1)], o <- [0..fromIntegral (p-1)]]
  where (w,h) = (10,10)


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "sigs.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInLine rectsx rectsy
        palette = rgbPalette' rectsx rectsy
        rectsx = 65
        rectsy = 65
        nrects = rectsx * rectsy
        (w,h) = (650,650) -- width,height da imagem SVG



