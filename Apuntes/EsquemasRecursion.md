# Funciones utiles de Haskell


#### Map
~~~
map f []     = []
map f (x:xs) = f x : map f xs 
~~~

#### Flip
~~~
flip f x y = f y x
~~~

#### Composicion de funciones
~~~
(.) f g x = f (g x)
~~~

#### Recursion estructural
~~~
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z 
foldr f z (x:xs) = f x (foldr f z xs)
~~~

#### Recursion iterativa
~~~
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ac [] = ac
foldl f ac (x:xs) = foldl f (f ac x) xs
~~~

#### Recursion primitiva
~~~
recr :: a -> (a -> [a] -> a) -> [a] -> a
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs)
~~~