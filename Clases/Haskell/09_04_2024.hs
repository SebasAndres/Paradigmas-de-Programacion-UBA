type Conj a = (a -> Bool)
vacio = const False

agregar :: Eq a => a -> Conj a -> Conj a
agregar e c = \x -> if x==e then True else c x

{-- TESTING

tmp = vacio
-- tmp 1 = False
tmp = agregar 1 tmp
-- tmp 1 = True
--}

pares :: Conj Int 
pares x = mod x 2 == 0
paresMas5 = agregar 5 pares

union :: Conj a -> Conj a -> Conj a
union c1 c2 = \x -> c1 x || c2 x

interseccion :: Conj a -> Conj a -> Conj a
interseccion c1 c2 = \x -> c1 x && c2 x

diferencia :: Conj a -> Conj a -> Conj a
diferencia c1 c2 = \x -> c1 x && not (c2 x)