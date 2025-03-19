# Final Plan Nuevo

## 1. Recursión estructural 

Escribir `recr` usando `foldr`:

~~~hs
-- FOLDR
foldr :: b -> (a -> b -> b) -> [a] -> b    
foldr z f [] = z
foldr z f (x:xs) = f x (foldr z f xs)

-- En la otra notación es esta ídea.. 
foldr' cEmpty cC l = case l of
    [] -> cEmpty
    x xs -> cC x (rec xs)
    where rec = foldr' cEmpty cC

-- RECR
recr' z f l = case l of 
    [] -> z
    x xs -> f x xs (rec xs)
    where rec = recr' z f l

-- El truco está en pasarle dos veces la lista!!! 
recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f xs = foldr z (\x r ys -> f x ys (r (tail ys))) xs xs
~~~

## 2. Inducción estructural y equivalencias
Sea 
~~~hs
dup f x = f x x
~~~

Demostrar usando las definiciones comunes de `map` y `zipWith`
~~~hs
map . dup = dup . zipWith 

map f [] = []
map f (x:xs) = f x : map f xs

-- zipWith ??
~~~

## 3. Deducción natural

Sea tⓧr una abreviacion de not(t => not(r)). 
Demostrar utilizando deduccion natural que esta abreviacion se comporta igual a la conjuncion, es decir:
> a. Si Γ-> t y Γ-> r, entonces Γ-> tⓧr (Introduccion)
> b. Si Γ-> tⓧr, entonces Γ-> t (Eliminacion 1)
> c. Si Γ-> tⓧr, entonces Γ-> r (Eliminacion 2)

## 4. Programación lógica
~~~pl
% a. subarbol(+A, -S). 
% Devuelve todos los subarboles de A
subtree(A, S) :- 
    sizeTree(A, N), % tamaño de arbol original
    generateNumberBelow(N, M), % tamaño del subarbol
    generateTree(M, S), % generamos un subarbol de tamaño M
    testSubtree(A, S).

% sizeTree(+a, -n)
sizeTree(Nil, 0).
sizeTree(Tree(I, X, D), N) :-
    sizeTree(I, NI),
    sizeTree(D, ND),
    max(I, D, Z),
    N is 1+Z.

% generate number < M
generateNumberBelow(M, N) :- desde(0, N), N < M.

% generate numbers that sum
generateNumbersThatSum(0, 0, 0).
generateNumbersThatSum(N, A, B) :- desde(0, A), B is N-A.

% generateTree(+N, -S)
generateTree(0, Nil).
generateTree(1, Tree(Nil, X, Nil)) :- isNat(X).
generateTree(N, Tree(I, X, D)) :-     
    generateNumbersThatSum(N, M1, M2),
    generateTree(M1, I),
    generateTree(M2, D),
    isNat(X).

% testSubtree(+a, +s)
testSubtree(Nil, Nil).
testSubtree(Tree(I, X, D), I). 
testSubtree(Tree(I, X, D), D). 
testSubtree(Tree(I, X, D), S) :- testSubtree(I, S). 
testSubtree(Tree(I, X, D), S) :- testSubtree(D, S). 

% b. tieneSubarbolMayorPeso(+A, +N). 
% Devuelve true si posee un subarbol cuyo peso es estrictamente mayor a N (el peso es la suma de los valores de los nodos del subarbol)
tieneSubarbolMayorPeso(A, N) :- 
    subtree(A, S),
    weight(S, P),
    P > N.

weight(Nil, 0).
weight(Tree(I, X, D), P) :- weight(I, PI), weight(D, PD), P is X+PI+PD.

% c. subarbolDeMayorPeso(+A, -S). 
% Devuelve todos los subarboles de mayor peso dentro de A.
subarbolDeMayorPeso(A, S) :- 
    subarbol(A, S),
    weight(S, WS),
    not(subarbol(A, J), weight(J, WJ), WJ > WS).
~~~

