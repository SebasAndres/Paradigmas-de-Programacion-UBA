% definir el predicado entre(+X,+Y,-Z) que sea verdadero 
% cuando el numero entero Z este comprendido entre los numeros X e Y inclusive.
% X<=Z<=Y

entre(X,Y,X) :- X =< Y.
entre(X,Y,Z) :- X<Y, X2 is X+1, entre(X2,Y,Z).

% sinRepetidos
scr([],[]).
scr([X],[X]).
scr([X,X|XS],L) :- scr([X|XS], L).
scr([X,Y|XS],[X|L]) :- X \= Y, scr([Y|XS],L).

% partes de una lista ~ recuerdo cardinalidad 2^N 
agregar_a_todos(X,[],[]). # elem a agregar, lista original, lista con elem agregado
agregar_a_todos(E,X,Z) :- X=[X0|XSS], Z=[Z0|ZSS], Z0=[E|X0], agregar_a_todos(E,XSS,ZSS).
partes([],[]).
partes([X|XS], Y) :- agregar_a_todos(X, XS, Y).

% solucion clase
% partes([],[]).
% partes([X|XS],[X,L]) :- partes(XS,L)
% partes([X|XS], L) :- partes(XS,L)

% prefijo(L,P) y sufijo(L,P)

append([],L,L). # agregar a [] L da L
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3)

prefijo(L,P) :- append(P,_,L). 

% sublista(L,SL) ~ tip usar prefijo y subfijo

% insertar(?X, +L, ?LX): LX se puede obtener insertando a X en alguna posicion de L (usando append)
insertar(X, L, LX) :- append(P, Z, LX), append(X,S,Z), prefijo(P,L), sufijo(S,L).

% version clase:
insertar(X,L,LX) :- append(I,D,L), append(I,[X|D], LX).

% permutacion(+L,?P) ~ usando append. tiene exito si L es unapermutacion de L
permutacion([],[]).
permutacion([X|XS], P) :- permutacion(XS,L), insertar(X,L,P).

% iesimo
iesimo(0,[X|_],X).
iesimo(I,[_|XS],X) :- iesimo(I2,XS,X), I is I2+1.