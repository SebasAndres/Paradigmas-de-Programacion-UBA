% var(X): Tiene éxito si X es una variable no instanciada en el momento de la llamada. 
% nonvar(X): Tiene éxito si X no es una variable no instanciada.
% ground(X): Tiene éxito si X es un término completamente instanciado, lo que significa
%             que X y todos sus sub-términos no contienen variables no instanciadas.

% desde2(+X, -Y)
desde2(X, X).
desde2(X, Y) :- var(Y), N is X+1, desde2(N, Y).
desde2(X, Y) :- nonvar(Y), X < Y.

% between(0,10,5). devuelve true, funciona tamb sin instanciar.

% esListaFinitaPositiva(-L)
esListaFinitaPositiva([]).
esListaFinitaPositiva(X) :- desde2(0, Y), sumlist(X,Y).

% generarLista(+suma, -lista)
generarLista(0,[]).
generarLista(S,[X|XS]) :- 
    S > 0, 
    desde2(1,X), % instancio x >= 1
    S1 is S-X,   % S1 = S-X
    generarLista(S1,XS). % sumlist(XS)=S-X=S1

% partes(+A,-P(A))
partes([],[]).
partes([X|XS],[X,L]) :- partes(XS,L)
partes([X|XS], L) :- partes(XS,L)

% sinRepetidos
scr([],[]).
scr([X],[X]).
scr([X,X|XS],L) :- scr([X|XS], L).
scr([X,Y|XS],[X|L]) :- X \= Y, scr([Y|XS],L).

% append
append([],L,L). 
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3)

% insertar
insertar(X,L,LX) :- append(I,D,L), append(I,[X|D], LX).

% permutacion(+L,?P) ~ usando append. tiene exito si L es unapermutacion de L
permutacion([],[]).
permutacion([X|XS], P) :- permutacion(XS,L), insertar(X,L,P).

% iesimo
iesimo(0,[X|_],X).
iesimo(I,[_|XS],X) :- iesimo(I2,XS,X), I is I2+1.

% paresMenoresQueX(+X,-Y)
pmq(X,Y) :- between(0,X,Y), Y mod 2 =:= 0.

% generarPares(X,Y)
paresSuman(S,X,Y) :- S1 is S-1, between(1,S1,X), Y is S-X. 
generarPares(X,Y) :- desde2(2,S), paresSuman(S,X,Y).

% coprimos(-X,-Y)
% 		            generate        test
coprimos(X,Y) :- generarPares(X,Y), gcd(X,Y)==1.

% EJ triangulos (tipo parcial) - programar perimetro usando esTriangulo(.)
esTriangulo(tri(A,B,C)) :- A < B+C, B < A+C, C < B+C.

% Vemos cada caso:
% T instanciada (o no) y P instanciada (o no)
% caso triangulo instanciado, P instanciado (o no)
perimetro(tri(A,B,C),P) :- ground(tri(A,B,C)), 
                           esTriangulo(tri(A,B,C)), P is A+B+C.

% caso triangulo no instanciado o parcialmente instanciado, P instanciado (o no)
perimetro(tri(A,B,C),P) :- not(ground(tri(A,B,C))), nonvar(P), 
                           armarTriplas(P,A,B,C), esTriangulo(tri(A,B,C)).

% desde2 no se cuelga cuando le pasas valores instanciados
% no poner desde2 en cualquier caso para evitar que se cuelgue (es para casos muy especificos)
armarTriplas(P,A,B,C) :- desde2(3,P), between(0,P,A), S is P-A, between(0,S,B), C is S-B.

natural(cero).
natural(suc(X)) :- natural(X).

menor(cero,suc(X)) :- natural(X).
menor(suc(X),suc(Y)) :- menor(X,Y).
