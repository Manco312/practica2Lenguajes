filtrarString :: [Char] -> [Char]
filtrarString code = filter (\c -> c /= '-') code

filtrarYConvertir :: String -> [Int]
filtrarYConvertir code = map (\c -> if c == 'x' then 10 else fromEnum c) (filtrarString code)

multiplicarCalculo :: String -> [Int]
multiplicarCalculo codeLista = zipWith (*) (filtrarYConvertir codeLista) [10,9..1] 

verificacionCodigo :: String -> Bool
verificacionCodigo listaProductos = mod (sum (multiplicarCalculo listaProductos)) 11 == 0
