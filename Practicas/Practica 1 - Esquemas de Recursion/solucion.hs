-----------------------------------------------------------------------------------------------------
-- Ejercicio 1
-- no curryficada
max2 :: (Float, Float) -> Float
max2 (x,y) | x >= y = x
           | otherwise = y

-- no curryficada
normaVectorial :: (Float, Float) -> Float
normaVectorial (x,y) = sqrt(x^2 + y^2)

-- curryficada
subtract' :: Float -> Float -> Float
subtract' = flip (-)

-- curryficada
predecesor :: Float -> Float
predecesor = subtract' 1
-- predecesor N = subtract 1 N = flip (-) 1 N = N - 1

-- curryficada
evaluarEnCero :: (Float -> a) -> a
evaluarEnCero = \f -> f 0

-- recuerdo ~ composicion de funciones f . g = \x -> f (g x)
-- curryficada
dosVeces :: (Float -> Float) -> Float -> Float
dosVeces = \f -> f . f 

-- curryficada
flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

-----------------------------------------------------------------------------------------------------
-- Ejercicio 2
-- definimos curry' y uncurry' porque ya estan definidas en Haskell
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c 
uncurry' f (x,y) = f x y 

-- No se puede hacer una funcion curryN generica

-----------------------------------------------------------------------------------------------------
-- Ejercicio 3

-- a) Usando foldr definir sum, elem, (++), filter y map
sum' :: [Int] -> Int
sum' = foldr (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' e = foldr (\ x recu -> x == e || recu) False 

-- (++') :: [a] -> [a] -> [a]
-- (++') = flip (foldr (:))

filter' p = foldr (\x y -> if p x then x:y else y) []

map' f = foldr (\x y -> f x : y) []

-- b) mejorSegun
mejorSegun :: (a-> a-> Bool)-> [a]-> a
mejorSegun f = foldr1 (\x recu -> if f x recu then x else recu) 

-- c) sumasParciales
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldr (\x recu -> x + (head recu) : recu) [0]

-- d) sumaAlt
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x recu -> x - recu) 0

sumaAlt' :: Num a => [a] -> a
sumaAlt' = foldl (\recu x -> x - recu) 0

-----------------------------------------------------------------------------------------------------
-- Ejercicio 4
-- obs: 
-- concatMap :: (a-> [b])-> [a]-> [b]


-- a) Permutaciones de una lista
remove :: Eq a => a -> [a] -> [a]
remove x = filter (/=x)

permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = concatMap (\x -> map (x:) (permutaciones (remove x xs))) xs

-- b) Partes de una lista
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = agregar x (partes xs) ++ partes xs
                where agregar x = map (x:)
-- partes ls = concatMap (\x -> map (x:) (partes (filter (/=x) ls))) ls

-- c) Prefijos
prefijos :: [a] -> [[a]]
prefijos [x] = [[]]
prefijos (x:xs) = [] : map (x:) (prefijos xs)

-- d) Sublistas
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = concatMap (\ys -> [x:ys, ys]) (sublistas xs)

-----------------------------------------------------------------------------------------------------
-- Ejercicio 5
-- Indicar si es recursion estructural, si lo es, reescribirlo con foldr

-- a) NO es recursion estructural, porque 
--    si bien el caso base es un valor fijo (una lista vacia),
--    el caso recursivo hace uso de `x` y `xs`, cuando solo puede hacer uso de  
--    `x` y `g(xs)`
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares []     = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
                                    else x : elementosEnPosicionesPares (tail xs)

-- b) SI es recursion estructural, porque
--    el caso base es un valor fijo (siempre devuelve la funcion identidad),
--    el caso recursivo hace uso de `x` y de `g(xs, ..)` 
entrelazar :: [a] -> [a] -> [a]
entrelazar []     = id
entrelazar (x:xs) = \ys ->
                        if null ys
                        then x : entrelazar xs []
                        else x : head ys : entrelazar xs (tail ys)

entrelazar' :: [a] -> [a] -> [a]
entrelazar' = foldr (\x recu -> 
                        \ys -> x : head 
                        ys : recu (tail ys))
                    id

-----------------------------------------------------------------------------------------------------
-- Ejercicio 6
-- Esquema recursion primitiva
recr :: (a-> [a]-> b-> b)-> b-> [a]-> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- a) SacarUna: eliminar primera aparicion de un elemento
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs recu -> if e == x then xs else x:recu) []  

-- b) El esquema foldr no es adecuado para sacaraUna porque no se puede acceder al valor de xs

-- c) insertarOrdenado: insertar un elemento en una lista ordenada manteniendo el orden creciente
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs recu -> if e<=x then e:x:xs else x:recu) [] 

-----------------------------------------------------------------------------------------------------
-- Ejercicio 7

-- Genera una lista de una cantidad dada de elementos, a partir de un elemento inicial y de una
-- función de incremento entre los elementos de la lista.
-- genLista x f n = [x, f x, f (f x), ..., f^n x]

genLista :: a -> (a -> a) -> Integer -> [a]
genLista x f n = foldr (\_ recu -> map f (x:recu)) [] [1..n]
 
-- Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor
-- que el segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo

desdeHasta :: Integer -> Integer -> [Integer] 
desdeHasta x y = genLista (x-1) (+1) (y-x+1) 

-----------------------------------------------------------------------------------------------------
-- Ejercicio 8

-- Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben
-- poder aplicarse a listas finitas e infinitas

-- a) mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
-- de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\x recu -> (uncurry f) x : recu) []

-- b) armarPares (ZIP), que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
-- correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
-- ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
-- Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial.
armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x recu -> \ys -> (x,head(ys)) : recu (tail ys)) (\ys -> [])

-- c) mapDoble (ZIPWITH), una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
-- (de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento correspondiente de
-- las dos listas. Esta función en Haskell se llama zipWith.
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x recu -> \ys -> (uncurry f) (x,head(ys)) : recu (tail ys)) (\ys -> [])

-----------------------------------------------------------------------------------------------------
-- Ejercicio 9
-- a) suma matricial con zipWith
sumaMat :: [[Int]]-> [[Int]]-> [[Int]]
sumaMat = zipWith (zipWith (+))

-- Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
-- en la posición i,j del resultado está el contenido de la posición j,i de la matriz original. Notar que si la
-- entrada es una lista de N listas, todas de longitud M, la salida debe tener M listas, todas de longitud N.
-- trasponer :: [[Int]]-> [[Int]]
