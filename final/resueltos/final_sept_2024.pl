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