{-

map f []     = []
map f (x:xs) = f x : map f xs 

flip f x y = f y x

(.) f g x = f (g x)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ac [] = ac
foldl f ac (x:xs) = foldl f (f ac x) xs
-}

-- Ejercicio 1

max2 :: (Float, Float) -> Float
max2 (x,y) | x >= y = x
           | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x,y) = sqrt(x^2 + y^2)

subtract :: Float -> Float -> Float
subtract = flip (-)

predecesor :: Float -> Float
predecesor = subtract 1
-- predecesor N = subtract 1 N = flip (-) 1 N = N - 1

evaluarEnCero :: (Float -> a) -> a
evaluarEnCero = \f -> f 0

-- recuerdo ~ composicion de funciones f . g = \x -> f (g x)
dosVeces :: (Float -> a) -> a -> Float
dosVeces = \f -> f . f 

flipAll = map flip
