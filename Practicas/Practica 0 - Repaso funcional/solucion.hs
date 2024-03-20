-- ejercicio 1
-- null :: [a] -> Bool                -- True si la lista es vacía
-- head :: [a] -> a                   -- Devuelve el primer elemento de la lista
-- tail :: [a] -> [a]                 -- Devuelve la lista sin el primer elemento
-- length :: [a] -> Int               -- Devuelve la longitud de la lista
-- init :: [a] -> [a]                 -- Devuelve la lista sin el último elemento
-- last :: [a] -> a                   -- Devuelve el último elemento de la lista
-- take :: Int -> [a] -> [a]          -- Devuelve los primeros n elementos de la lista
-- drop :: Int -> [a] -> [a]          -- Devuelve la lista sin los primeros n elementos
-- (++) :: [a] -> [a] -> [a]          -- Concatena dos listas
-- concat :: [[a]] -> [a]             -- Concatena una lista de listas
-- (!!) :: [a] -> Int -> a            -- Devuelve el n-ésimo elemento de la lista
-- elem :: Eq a => a -> [a] -> Bool   -- True si el elemento pertenece a la lista

-- ejercicio 2
-- 1
valorAbsoluto :: Float -> Float
valorAbsoluto x | x >= 0 = x
                | otherwise = -x 
-- 2
bisiesto :: Int -> Bool
bisiesto n | mod n 4 == 0 && mod n 100 /= 0 = True
           | mod n 400 == 0 = True
           | otherwise = False

--3
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 4
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = length (filter esPrimo (divisores x))

esPrimo :: Int -> Bool
esPrimo n = length (divisores n) == 2

divisores :: Int -> [Int]
divisores x = divisoresDesde x 1 

divisoresDesde :: Int -> Int -> [Int]
divisoresDesde x d | d==x = [x] 
                   | mod x d == 0 = (d: divisoresDesde x (d+1))
                   | otherwise = divisoresDesde x (d+1)

-- ejercicio 3
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

--a 
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

--b
aEntero :: Either Int Bool -> Int 
aEntero (Left x) = x
aEntero (Right x) | x = 1
                  | otherwise = 0

-- ejercicio 4
-- a
limpiar :: String -> String -> String
limpiar a [] = []
limpiar a (y:ys) | elem y a = limpiar a ys
                 | otherwise = y : limpiar a ys

-- b
difPromedio :: [Float] -> [Float]
difPromedio ls = map ((+) k) ls
               where k = -avg ls

avg :: [Float] -> Float
avg [] = 0
avg ls = (suma ls) / fromIntegral (length ls)

suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs 

-- c
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:xs) = filter (==x) xs == xs

-- ejercicio 5
data AB a = Nil | Bin (AB a) a (AB a)

-- a
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

--b 
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i x d) = Bin (negacionAB i) (not x) (negacionAB d)

--c
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i x d) = (productoAB i) * x * (productoAB d)

-- EXTRA
mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ [x] = [x]
mergesort f ls = merge f (mergesort f (take mid ls)) (mergesort f (drop mid ls))
		         where mid = div (length ls) 2

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] y = y
merge _ x [] = x
merge f (x:xs) (y:ys) | f x y = x : merge f xs (y:ys)
		              | otherwise = y : merge f (x:xs) ys

-- relacion de ejemplo
relacion :: Int -> Int -> Bool
relacion x y = x < y