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
trasponer :: [[Int]] -> [[Int]]
trasponer = foldr (\x recu -> zipWith (:) x recu) (repeat [])

-----------------------------------------------------------------------------------------------------
-- Ejercicio 10
generate :: ([a]-> Bool)-> ([a]-> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom:: ([a]-> Bool)-> ([a]-> a)-> [a]-> [a]
generateFrom stop next xs | stop xs = init xs
                        | otherwise = generateFrom stop next (xs ++ [next xs])

-- i) 
--              stop            z    next(last)   out
generateBase :: ([a]-> Bool) -> a -> (a -> a) -> [a]
generateBase stop z next = generateFrom' stop z next [] 

generateFrom':: ([a]-> Bool)-> a -> (a -> a)-> [a]-> [a]
generateFrom' stop z next xs | stop xs = init xs
                | otherwise = generateFrom' stop z next (xs ++ [casesIni xs])
                where casesIni xs = if length xs > 0 then next (last xs) else z
-- ii)
factoriales :: Int -> [Int]
factoriales n = generate (\xs -> length xs == n+1) (\xs -> if length xs > 0 then last xs * (length xs + 1) else 1)
                         -- stop                   -- next

-- iii) 
iterateN :: Int-> (a-> a)-> a-> [a]
iterateN n f x = generateBase (\xs -> length xs == n+1) x (\y -> f y) 


-- iv) Redefinir generateFrom usando `iterate` y `takeWhile`
-- generateFrom'' :: ([a]-> Bool)-> ([a]-> a)-> [a]-> [a]
-- generateFrom'' stop next xs = takeWhile (not . stop) (iterate . next xs)

-----------------------------------------------------------------------------------------------------
-- Ejercicio 11
--  Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de
--  Haskell (la función va a estar definida sólo para los enteros mayores o iguales que 0).

--          f(x, recu)            z     n         OUT
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

-- Ejemplo de uso: Suma de los primeros n naturales
-- sumaNat :: Integer -> Integer
-- sumaNat n = foldNat (+) 0 n

potencia :: Integer -> Integer -> Integer
potencia n k = foldNat (\x recu -> n*recu) 1 k

-----------------------------------------------------------------------------------------------------
-- Ejercicio 12
data Polinomio a = X 
                    | Cte a 
                    | Suma (Polinomio a) (Polinomio a) 
                    | Prod (Polinomio a) (Polinomio a)

foldPoli :: Num b => b -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli cX cCte cSum cProd p = case p of 
    X		    -> cX
    Cte c 		-> cCte c
    Suma p q 	-> cSum (rec p) (rec q)
    Prod p q 	-> cProd (rec p) (rec q)
    where rec = foldPoli cX cCte cSum cProd

evaluar :: Num a => a -> Polinomio a -> b
evaluar n = foldPoli n id (+) (*)

-----------------------------------------------------------------------------------------------------
-- Ejercicio 13
data AB a = Nil | Bin (AB a) a (AB a)

-- recursion estructural
-- foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
-- foldAB fNil fTree t = case t of 
--     Nil         -> fNil
--     (Bin i r d) -> fTree (recu i) r (recu d)
--     where recu = foldAB fNil fTree 

-- recursion primitiva
-- recrAB :: (a -> b -> b -> b) -> b -> AB a -> b
-- recrAB f z Nill = z
-- recrAB f z (Bin i r d) = f r (recrAB f z i) (recrAB f z d)

esNil :: AB a -> Bool
esNil t = case t of 
    Hoja h -> True
    _ 	-> False

--altura :: AEB a -> Int
--altura = foldAEB ((+) 1) (\i r d -> if d > i then d+1 else i+1)

-- cantNodos :: AEB a -> Int    
-- cantNodos = foldAEB (const 1) (\i r d -> i + 1 + d)

-----------------------------------------------------------------------------------------------------
-- Ejercicio 14


-----------------------------------------------------------------------------------------------------
-- Ejercicio 15
data AEB a = Hoja a | Bin (AEB a) a (AEB a)

foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b
foldAEB fHoja fBin (Hoja h)            = fHoja h 
foldAEB fHoja fBin (Bin izq root der)  = fBin (rec izq) root (rec der)
        where rec = foldAEB fHoja fBin