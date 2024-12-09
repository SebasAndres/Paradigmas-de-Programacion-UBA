% Clase 18/06/2024

% obs. `make.` recompila el archivo cargado
sumlist([],0).
sumlist([X|XS],Y) :- sumlist(XS, S), Y is S+X.

% ej. 1 (desde cuando llegue es el 1ro, hay mas)

% sol clase
iesimo(0,[X|_],X).
iesimo(I,[_|XS],X) :- iesimo(I2,XS,X), I is I2+1.

% probar
% iesimo(_,[],_).
% iesimo(0,X|XS,X).
% iesimo(suc(N), X|XS, Y) :- iesimo(N, XS, Y).

% ej. 2
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

desde2(X,X).
desde2(X,Y) :- var(Y), N is X+1, desde2(N,Y).
desde2(X,Y) :- nonvar(Y), Y>X.

% ej. 3
% perdicado pmq(+X,-Y) que genera todos los naturales pares menores o iguales a X
pmq(X,Y) :- between(0,X,Y), Y mod 2 =:= 0.
% donde between(0,X,Y): Y es un numero que esta en la lista de numeros del rango 0 --> X.

% intentar hacerlo recursivamente
% ...


% ej. 4.
% Esquema general de Generate & Test
% coprimos(-X,-Y)
paresSuman(S,X,Y) :- S1 is S-1, between(1,S1,X), Y is S-X. % se define asi para que ninguna tupla tenga un 0 ~ creo
generarPares(X,Y) :- desde2(2,S), paresSuman(S,X,Y).
% 		            generate        test
coprimos(X,Y) :- generarPares(X,Y), gcd(X,Y)==1.

% ej. 5
cmp(L, L1, L2) :- unCorte(L,L1,L2,D), not((unCorte(L,_,_,D2), D2<D)).
unCorte(L,L1,L2,D) :- append(L1,L2,L), sumlist(L1,S1), sumlist(L2,S2), D is abs(S1-S2). 

% ej 6. triangulos (tipo parcial) - programar perimetro usando esTriangulo(.)
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
