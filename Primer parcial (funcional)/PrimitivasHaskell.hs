null :: [a] -> Bool                                  -- True si la lista es vacía
head :: [a] -> a                                     -- Devuelve el primer elemento de la lista
tail :: [a] -> [a]                                   -- Devuelve la lista sin el primer elemento
length :: [a] -> Int                                 -- Devuelve la longitud de la lista
init :: [a] -> [a]                                   -- Devuelve la lista sin el último elemento
last :: [a] -> a                                     -- Devuelve el último elemento de la lista
take :: Int -> [a] -> [a]                            -- Devuelve los primeros n elementos de la lista
drop :: Int -> [a] -> [a]                            -- Devuelve la lista sin los primeros n elementos
(++) :: [a] -> [a] -> [a]                            -- Concatena dos listas
concat :: [[a]] -> [a]                               -- Concatena una lista de listas
(!!) :: [a] -> Int -> a                              -- Devuelve el n-ésimo elemento de la lista
elem :: Eq a => a -> [a] -> Bool                     -- True si el elemento pertenece a la lista
zip :: [a] -> [b] -> [(a, b)]                        -- Combina dos listas en una lista de pares
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]        -- Combina dos listas con una función
repeat :: a -> [a]                                   -- Repite un elemento infinitamente
iterate :: (a -> a) -> a -> [a]                      -- Aplica una función infinitamente
takeWhile :: (a -> Bool) -> [a] -> [a]               -- Devuelve los elementos que cumplen la condición
dropWhile :: (a -> Bool) -> [a] -> [a]               -- Devuelve la lista sin los elementos que cumplen la condición
filter :: (a -> Bool) -> [a] -> [a]                  -- Devuelve los elementos que cumplen la condición
map :: (a -> b) -> [a] -> [b]                        -- Aplica una función a todos los elementos de la lista
