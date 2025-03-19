
% Utilizo el esquema Generate & Test.

% listasGemelas(-L1, -L2): 
% Vale cuando el concatenarlas, el resultado es capicúa
listasGemelas([], []).
listasGemelas(A, B) :- 
    desde(0, N), % genero la longitud de las listas capicuas
    generarListaTam(N, A), % genero una lista de tamaño N
    generarListaTam(N, B), % genero otra lista de tamaño N
    sonEspejo(A, B).       % testeo la condicion

% generarListaTam(+N, -A)
generarListaTam(0, []).
generarListaTam(N, [X|XS]) :- 
    N > 0, 
    member(X, [0,1]),
    M is N-1,
    generarListaTam(M, XS).

% sonEspejo(+X, ?Y)
sonEspejo([], []).
sonEspejo([X], [X]).
sonEspejo([X|XS], [Y|YS]) :- 
    tail(YS, X),
    tail(XS, Y),
    sacarCola([Y|YS], L),
    sonEspejo(XS, L).

% desde(+X, -Y)
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).

% tail(+L, -X)
tail([X], X).
tail([X|XS], Y) :- tail(XS, Y).

% sacarCola(+L, -L')
sacarCola([X], []).
sacarCola([X|XS], [X|YS]) :- sacarCola(XS, YS).
