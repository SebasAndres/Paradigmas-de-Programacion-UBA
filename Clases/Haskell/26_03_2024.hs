
-- ######################################################################

-- definimos curry' y uncurry' porque ya estan definidas en Haskell
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)
-- curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c 
uncurry' f (x,y) = f x y 

prod :: Int -> Int -> Int
prod x y = x * y 

-- 
doble :: Int -> Int
doble x = prod 2 x 

-- funciona igual, es por "aplicacion parcial"
doble2 :: Int -> Int
doble2 = prod 2 

-- definir funciones de forma prefija, similar a (+) 1
triple :: Float -> Float
triple = (*) 3

esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (<) 18
-- esMayorDeEdad x = 18 < x
-- esMayorDeEdad = (>=18)

composicion :: (b -> c) -> (a -> b) -> a -> c 
composicion f g x = f (g x)
-- composicion f g = \x -> f g x

flip_ :: (a -> b -> c) -> b -> a -> c 
flip_ f = \x y -> f y x

($$) :: (a->b) -> a -> b 
($$) f a = f a

const_ :: a -> (b -> a)
const_ z = (\ x -> z)

-- evalua en 0 la funcion
-- ($) f x, f: funcion, x: valor
-- flip ($) 0 = flip (($) 0 f) = ($) f 0 = f 0 
evaluarEnCero = flip ($) 0

esPar = (==0) . (flip mod 2)
-- esPar x  = (==0) (mod x 2)

-- ######################################################################
-- DEFINICION DE LISTAS:

-- [1]. Por extension
listaDeABC = ['a', 'b', 'c']

-- [2]. Secuencias
listaHasta10 = [0..10]

-- [3]. Por comprension
-- [expresion | selectores, condiciones]
listaDeTuplasQueSuman4 = [(x,y) | x <- [0..5], y <- [0..3], x+y==4]
listaDeParesMenoresA10 = [x | x <- [0..10], mod x 2 == 0]

-- ######################################################################
-- EVALUACION LAZY
-- si para algun termino existe una reduccion finita, entoncs la estrategia 
-- de reduccion lazy termina

-- take :: Int -> [a] -> [a]
-- take 0 = []
-- take [] = []
-- take n (x:xs) = x : take (n-1) xs

-- ######################################################################
-- FUNCIONES DE ALTO ORDEN

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f [x]     = x
mejorSegun f (x:xs) | f x (mejorSegun f xs) = x 
                    | otherwise = mejorSegun f xs

maximo :: Ord a => [a] -> a 
maximo = mejorSegun (\x y -> x>y)

minimo :: Ord a => [a] -> a 
minimo = mejorSegun (\x y -> y>x)

listaMasCorta :: [[a]] -> [a]
listaMasCorta = mejorSegun (\ x y -> length x < length y)

-- ######################################################################
-- FILTER
deLongitudN :: Int -> [[a]] -> [[a]] -- devuelve listas de long N
deLongitudN n = filter (\x -> length x == n)

soloPuntosFijosEnN :: Int -> [Int -> Int] -> [Int -> Int] -- devuelve funciones tales que f(n)=n
soloPuntosFijosEnN n = filter (\f -> f(n) == n) 

-- ######################################################################
-- MAP
reverseAnidado :: [[Char]] -> [[Char]] -- da vuelta cada string anidado
reverseAnidado = reverse . (map invertirPalabra) 
-- equivale a ...
-- reverseAnidado ls = reverse (map invertirPalabra ls) 

invertirPalabra :: [Char] -> [Char]
invertirPalabra []     = []
invertirPalabra (x:xs) = invertirPalabra xs ++ [x] 

paresCuadrados :: [Int] -> [Int]
paresCuadrados = map (\x -> if mod x 2 == 0 then x^2 else x) 

-- ######################################################################
-- DESPLEGANDO LA MACRO DE LAS LISTAS POR COMPRESION
-- definir equivalente a 
-- listaComp f x p = [f x | x <- xs, p x]
-- con map y filter

listaComp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listaComp f p = (map f) . (filter p)
-- listaComp f p xs = map f (filter p xs) ## equivalente

-- ######################################################################
-- ESQUEMAS DE RECURSION SOBRE LISTAS
{-

Para situaciones en las cuales no hay un caso base claro (ej: no existe el neutro),
tenemos las funciones: foldr1 y foldl1.

Permiten hacer recursi´on estructural sobre listas sin definir un caso base:
* foldr1 toma como caso base el ´ultimo elemento de la lista.
* foldl1 toma como caso base el primer elemento de la lista.

Para ambas, la lista no debe ser vac´ıa, y el tipo del resultado debe
ser el de los elementos de la lista

INTERPRETO:

foldr1 :: (a -> b -> b) -> b -> [a] -> b
foldr1 f z xs | z == xs   = z
              | otherwise = f x (foldr f z xs)

RECUERDO:

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ac [] = ac
foldl f ac (x:xs) = foldl f (f ac x) xs

recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

REESCRIBIR mejorSegun:

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f [x]     = x
mejorSegun f (x:xs) | f x (mejorSegun f xs) = x 
                    | otherwise = mejorSegun f xs

-}

mejorSegun' :: (a -> a -> Bool) -> [a] -> a 
mejorSegun' f = foldr1 (\x y -> if f x y then x else y)
-- equivale a 
-- mejorSegun' f ls = foldr (\x y -> if f x y then x else y) (head ls) ls
-- funciona tambien con foldl1 (porque f es un operador asociativo y conmutativo, es decir,
-- no importa el orden, debe dar igual)  
-- mejorSegun' f = foldl1 (\x y -> if f x y then x else y)

-- ######################################################################
-- ESQUEMAS DE RECURSION SOBRE LISTAS

-- elem:
-- llamo recu al resultado de la recursion
elem' :: Eq a => a -> [a] -> Bool
elem' e = foldr (\ x recu -> x == e || recu) False 

-- sumaAlt:
-- realiza una suma alternada de la lista f [1,2,3] = +1-2+3
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0  
-- creo que equivale a 
-- sumaAlt ls = foldr (\ x recu -> x - recu) 0 ls

-- sacarPrimera: 
-- elimina la primera aparicion de un elemento en la lista
sacarPrimera :: Eq a => a -> [a] -> [a] 
sacarPrimera e = recr (\x xs recu -> if e==x then recu else x:recu) []

recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- ######################################################################
-- ESQUEMAS DE RECURSION SOBRE NUEVAS ESTRUCTURAS :: ARBOLES E. BINARIOS

-- arboles estrictamente binarios (AEB)
data AEB a = Hoja a | Bin (AEB a) a (AEB a)

-- definir el esquema de recursion estructural para arboles estrictamente binarios
-- El esquema debe permitir definir las funciones altura, ramas, #nodos, #hojas, espejo, etc.
foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b
--           fHoja  -> fBin               -> arbol -> res 
foldAEB fHoja fBin (Hoja h)            = fHoja h 
foldAEB fHoja fBin (Bin izq root der)  = fBin (rec izq) root (rec der)
        where rec = foldAEB fHoja fBin

-- equivale a escribir...

-- foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b
-- foldAEB fHoja fBin t = case t of
--  Hoja n -> fHoja n
--  Bin t1 n t2 -> fBin (rec t1) n (rec t2)
--  where rec = foldAEB fHoja fBin

-- Tarea: Escribir funciones altura, ramas, #nodos, #hojas y espejo con foldAEB
cantNodos :: AEB a -> Int
cantNodos = foldAEB (const 1) (\ rizq _ rder -> 1 + rizq + rder) 

altura :: AEB a -> Int
altura = foldAEB (const 1) (\ rizq _ rder -> 1 + max rizq rder)

-- ######################################################################
-- ESQUEMAS DE RECURSION SOBRE NUEVAS ESTRUCTURAS :: POLINOMIOS
data Polinomio a = X
                | Cte a
                | Suma (Polinomio a) (Polinomio a)
                | Prod (Polinomio a) (Polinomio a)

evaluar :: Num a => a -> Polinomio a -> a
-- evaluar x t = case t of 
--     Cte k                            -> k
--     Suma p q -> evaluar x p + evaluar x q 
--     Prod p q -> evaluar x p * evaluar x q 

--          cX -> fCte    -> fSuma         -> fProd          -> input       -> res
foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli cX fCte fSuma fProd poli = case poli of 
    X           -> cX  
    Cte k       -> fCte k 
    Suma p q    -> fSuma (rec p) (rec q)
    Prod p q    -> fProd (rec p) (rec q)
    where rec = foldPoli cX fCte fSuma fProd

-- escrito con el esquema de recursion estructural
evaluar n = foldPoli n id (+) (*)

-- ######################################################################
-- ESQUEMAS DE RECURSION SOBRE NUEVAS ESTRUCTURAS :: ROSE TREE
data RoseTree a = Rose a [RoseTree a]

tamañoRT :: RoseTree a -> Int 
tamañoRT (Rose x hijos) = 1 + sumlist (map tamañoRT hijos)

--              f         ->    input   -> res 
foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT f (Rose x hijos) = f x (map (foldRT f) hijos)

tamañoRT' = foldRT (\_ recs -> 1 + sumlist recs)

