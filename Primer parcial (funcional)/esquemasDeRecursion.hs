--Esquemas de recursion

--Estructural
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

--Iterativa
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ac [] = ac
foldl f ac (x:xs) = foldl f (f ac x) xs

--Primitiva
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)
 