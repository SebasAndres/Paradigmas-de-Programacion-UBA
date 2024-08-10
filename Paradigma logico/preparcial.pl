% Ej. PESOS
peso([],0).
peso([X],Y) :- peso(X,Y).
peso([X|XS], P) :- peso(X,P1), peso(XS,P2), P is P1+P2.
peso(X,X).

pesoMaximo([],0).
pesoMaximo([X|XS], P) :- peso(X,P2), pesoMaximo(XS,P1), P is max(P2,P1).

elementoMasPesado(L,X) :- member(X,L), pesoMaximo(L,P), peso(X,P).

% EJ. Arboles
% camino(+A, -C)
% C es un camino de la raiz a alguna hoja
camino(bin(nil,V,nil), [V]).
camino(bin(I,V,D),[V|XS]) :- camino(D,XS), D \= nil.
camino(bin(I,V,D),[V|XS]) :- camino(I,XS), I \= nil. 

% caminoMasLargo(+A, -C)
caminoMasLargo(A,C) :- camino(A,C),
                       length(C,L1),
                       not(
                            (camino(A,C2),
                             length(C2,L2),
                             L2 > L1
                            )
                        ).                                

% caminoUnicoDeLong(+A, +N, -C)                    
% si C es un camino de A de longitud N y no hay otro camino de longitud N
caminoUnicoDeLong(A,N,C) :- camino(A,C),
                            length(C,N),
                            not(
                                (camino(A,C2),
                                 length(C2,N),
                                 C \= C2
                                )
                            ).

% palabra(+A,+N, ?P), al final P nos quedo reversible
% generar palabras de longitud N de un alfabeto A
palabra(A,0,[]).
palabra(A,N,[X|XS]) :- N >= 0, member(X,A), N1 is N-1, palabra(A,N1,XS).

% frase(+A,-F)
% una frase es un a lista finita de palabras no vacias en A generar todas las frases posibles
frase(_,[]).
frase(A,F) :- desde2(1,N), fraseSumaX(A,N,F).

% fraseSumaX(+A,+N,-F)
fraseSumaX(A,N,[F]) :- palabra(A,N,F).
fraseSumaX(A,N,[F|FS]) :- N>0, between(1,N,N1), palabra(A,N1,F), N2 is N-N1, N2>0, fraseSumaX(A,N2,FS).

% desde
desde2(X, X).
desde2(X, Y) :- var(Y), N is X+1, desde2(N, Y).
desde2(X, Y) :- nonvar(Y), X < Y.

% EJ ochoReinas(+XS)
% la posicion en la lista es la columna y el numero es la fila
ochoReinas([]).
ochoReinas([X|XS]) :- ochoReinasAux([X|XS], 1).

% C representa el indice de R
ochoReinasAux([R|XS],C) :- between(1,8,R),
                           not(
                            (member(R1,XS),
                            encontrarIndice(R1,XS,C1), % C1 es el indice de R1 en XS.
                            C2 is C+C1,
                            colision(R,C,R1,C1))
                            ),
                            ochoReinas(XS).

encontrarIndice(X,[X|_], 1).
encontrarIndice(X,[Y|XS],P) :- encontrarIndice(X,XS,P2), P is P2+1.

colision(R,_,R,_).
colision(_,C,_,C).
colision(R1,C1,R2,C2) :- between(1,8,Z), R2 is R1+Z, C2 is C1+Z. 

%%%%%%%%%%5
repeticiones(X,[],0).
repeticiones(X,[X|XS],Z) :- repeticiones(X,XS,Z2), Z is Z2+1.
repeticiones(X,[Y|XS],Z) :- X\=Y, repeticiones(X,XS,Z).