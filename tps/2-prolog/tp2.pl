% Pedro Fuentes Urfeig 1088/22
% Juan Manuel Zimmerman 123/23
% Sebastian Andres 1088/22
% Vicente Tenconi 1171/22

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

tablero(ej5x5, T) :-
    tablero(5, 5, T),
    ocupar(pos(1, 1), T),
    ocupar(pos(1, 2), T).

tablero(libre20, T) :-
    tablero(20, 20, T).

%% Ejercicio 1 %%

%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

tablero(0, _, []).
tablero(_, 0, []).
tablero(F, C, [Fila|Resto]) :-
    F > 0,
    C > 0,
    F1 is F - 1,
    crear_fila(C, Fila),
    tablero(F1, C, Resto).

% crear_fila(+Columnas, -Fila) instancia una fila de celdas libres
crear_fila(0, []).
crear_fila(M, [_|Resto]) :-
    M > 0,
    M1 is M - 1,
    crear_fila(M1, Resto).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.

ocupar(pos(F,C), Tablero) :- 
    indicesValidos(F,C,Tablero),
    nth0(F, Tablero, Fila),
    nth0(C, Fila, ocupada).  

% indicesValidos(+F, +C, +Tablero) es verdadero si (F,C) es válido en Tablero
indicesValidos(F, C, Tablero) :-
    length(Tablero, N),
    F >= 0,
    C >= 0,
    F < N,
    nth0(F, Tablero, Fila),
    length(Fila, M),
    C < M.

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
% N es el length del tablero 

% derecha
vecino(pos(F, C), Tablero, pos(F1, C)) :- 
    F1 is F + 1,
    length(Tablero, N),
    F1 < N.
% abajo
vecino(pos(F, C), Tablero, pos(F, C1)) :- 
    C1 is C + 1,
    length(Tablero, N),
    C1 < N.
% izquierda
vecino(pos(F, C), _, pos(F1, C)) :-
    F1 is F - 1,
    F1 >= 0.
% arriba
vecino(pos(F, C), _, pos(F, C1)) :-
    C1 is C - 1,
    C1 >= 0.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(F, C), T, pos(F1,C1)) :-
    vecino(pos(F, C), T, pos(F1, C1)),
    estaLibre(pos(F1, C1), T).

% Pos es reversible!
% estaLibre(+Pos, +Tablero) es verdadero si la celda en Pos esta libre
estaLibre(pos(F, C), T) :-
    nth0(F, T, Fila),
    nth0(C, Fila, Celda),
    var(Celda). % Celda es de la forma _

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

camino(Inicio, Fin, Tablero, Camino) :-
    estaLibre(Inicio, Tablero),
    estaLibre(Fin, Tablero), 
    caminoAux(Inicio, Fin, Tablero, [Inicio], Camino).

% caminoAux(+Actual, +Fin, +Tablero, +Visitados, -Camino)
% caminoAux es exitoso si el Camino llega a Fin mediante celdas transitables
caminoAux(Fin, Fin, _, Visitados, Camino) :-
    reverse(Visitados, Camino).

caminoAux(Actual, Fin, Tablero, Visitados, Camino):-
    vecinoLibre(Actual, Tablero, Siguiente), % en alguna iteracion Siguiente va a ser Fin ->> Fin esta libre
    not(member(Siguiente, Visitados)),
    caminoAux(Siguiente, Fin, Tablero, [Siguiente|Visitados], Camino). 

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

% Fin es reversible, es decir, puede estar instanciado o no. Si no esta instanciado, es generado por estaLibre. Si no, estaLibre actua como verificador.
% Luego, es usado en caminoAux, pero para este momento ya esta instanciado, y así funciona correctamente, generando un 'Siguiente' hasta este sea unificable con 'Fin', en la primera linea de caminoAux.

% Camino es reversible en este predicado. Vemos que caminoAux/5 va a construir un camino posible y luego chequeara si la lista de visitados con reverse es igual al camino
% Si el camino es correcto, en algun momento caminoAux va a generar la lista de visitados que le corresponde a ese camino.

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones se instancien en orden creciente de longitud.

camino2(Inicio, Fin, Tablero, Camino) :- 
    cantVertices(Tablero, N), 
    between(0,N,K), 
    caminoConLong(Inicio, Fin, Tablero, Camino, K).

% usamos la cantidad de vertices como una cota superior de la long maxima de un camino simple.
% esto es porque no puede pasar dos veces por el mismo vertice
cantVertices([F|T], R) :- length([F|T], N), length(F, M), R is N*M.


%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

% Vemos que Inicio es reversible en camino2/4. Esto se debe a que es reversible en caminoConLong, que utiliza camino/4, y Inicio
% es reversible en este predicado, porque si no viene instanciado es generado por estaLibre/2.
% Entonces, si viene instanciado estaLibre lo verifica, y si no viene instanciado, estaLibre lo genera. Luego, es usado en caminoAux ya instanciado.

% Vemos que Camino tambien es reversible en este predicado.
% Si Camino ya viene instanciado (y es correcto), se van a generar las longitudes hasta dar con la que es la del camino que vino instanciado
% es decir, que en caminoConLong se va a chequear, primero que ese sea un camino, y luego que sea de esa longitud.
% vemos que si a camino le pasamos un camino correcto ya instanciado, va a devolver true.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(Inicio,Fin,Tablero,C1) :- 
    camino(Inicio, Fin, Tablero, C1), % Generate
    length(C1, L1), % Test
    not((caminoConLong(Inicio, Fin, Tablero, _, L2), L2<L1)). % Test

% caminoConLong(+Inicio, +Fin, +Tablero, -Camino, -L) es verdadero si Camino es un camino con longitud L 
caminoConLong(Inicio, Fin, Tablero, C2, L2) :- 
    camino(Inicio, Fin, Tablero, C2), % Generate
    length(C2, L2). % Test

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Camino es un camino desde Inicio hasta Fin pasando sólo por celdas transitables en ambos tableros.

% es un camino dual si es un camino valido en T1, y ademas es camino valido en T2.
caminoDual(Inicio, Fin, T1, T2, Camino) :-
    camino(Inicio, Fin, T1, Camino),
    camino(Inicio, Fin, T2, Camino).

%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(8). 
testTablero(1) :- tablero(0,0,[]).

% crear_fila
testTablero(2) :- crear_fila(3, [_,_,_]).
testTablero(3) :- not(crear_fila(0, [_,_,_])).

% ocupar
testTablero(4) :- ocupar(pos(0,0), [[ocupada]]).
testTablero(5) :- not(ocupar(pos(2,1),[[_,_],[_,_]])).
testTablero(6) :- tablero(2,2,T), ocupar(pos(0,0), T), T = [[ocupada,_],[_,_]].

% indicesValidos
testTablero(7) :- indicesValidos(0, 0, [[_,_]]).
testTablero(8) :- not(indicesValidos(1, 1, [[_,_]])).

cantidadTestsVecino(8).
% vecino
testVecino(1) :- tablero(2,3, T), vecino(pos(0,0), T, pos(0,1)), T = [[_,_,_],[_,_,_]].
testVecino(2) :- tablero(2,3, T), vecino(pos(0,0), T, pos(1,0)), T = [[_,_,_],[_,_,_]].
testVecino(3) :- tablero(2,3, T), vecino(pos(0,0), T, pos(0,1)), T = [[_,_,_],[_,_,_]].
testVecino(4) :- tablero(2,3, _), not(vecino(pos(1,1), _, pos(0,0))). 
% vecinoLibre
testVecino(5) :- tablero(2,3, T), vecinoLibre(pos(0,0), T, _).
testVecino(6) :- tablero(2,3, T), ocupar(pos(1,0), T), vecinoLibre(pos(0,0), T, _).
testVecino(7) :- tablero(2,3, T), ocupar(pos(0,1), T), ocupar(pos(1,0), T), not(vecinoLibre(pos(0,0), T, _)).
testVecino(8) :- not((tablero(2,3, T), ocupar(pos(1,0), T), vecinoLibre(pos(0,0), T, pos(1,0)))). 

cantidadTestsCamino(3).
% camino
testCamino(1) :- tablero(2,2,T), camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)]).
testCamino(2) :- tablero(2,2,T), camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)]).
testCamino(3) :- tablero(2,2,T), ocupar(pos(1,0), T), ocupar(pos(0,1), T), not(camino(pos(0,0), pos(1,1), T, _)).

cantidadTestsCaminoAux(5).

% caminoAux
testCaminoAux(1) :- tablero(2, 2, T), caminoAux(pos(0, 0), pos(1, 1), T, [pos(0, 0)], [pos(0, 0), pos(0, 1), pos(1, 1)]).
testCaminoAux(2) :- tablero(2, 2, T), caminoAux(pos(0, 0), pos(1, 1), T, [pos(0, 0)], [pos(0, 0), pos(1, 0), pos(1, 1)]).
testCaminoAux(3) :- tablero(2, 2, T), ocupar(pos(1, 0), T), ocupar(pos(0, 1), T), not(caminoAux(pos(0, 0), pos(1, 1), T, [pos(0, 0)], _)).
testCaminoAux(4) :- tablero(3, 3, T), ocupar(pos(1, 1), T), caminoAux(pos(0, 0), pos(2, 2), T, [pos(0, 0)], [pos(0, 0), pos(0, 1), pos(0, 2), pos(1, 2), pos(2, 2)]).
testCaminoAux(5) :- tablero(3, 3, T), ocupar(pos(1, 1), T), not(caminoAux(pos(0, 0), pos(2, 2), T, [pos(0, 0)], [pos(0, 0), pos(1, 0), pos(1, 1), pos(2, 2)])).


cantidadTestsCamino2(2).
% camino2
testCamino2(1) :- tablero(3,3,T), camino2(pos(0,0), pos(2,2), T, [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,2)]).
testCamino2(2) :- tablero(3,3,T), ocupar(pos(1,1), T), camino2(pos(0,0), pos(2,2), T, [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,2)]).

cantidadTestsCantVertices(2).
% cantVertices
testCantVertices(1) :- tablero(2,2,T), cantVertices(T, 4).
testCantVertices(2) :- tablero(ej5x5, T), cantVertices(T, 25).

cantidadTestsCaminoOptimo(6).
% caminoOptimo
testCaminoOptimo(1) :- tablero(3,2,T), caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)]).
testCaminoOptimo(2) :- tablero(3,2,T), caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)]).
testCaminoOptimo(3) :- tablero(3,2,T), ocupar(pos(1,0), T), not(caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)])).
testCaminoOptimo(4) :- tablero(3,2,T), ocupar(pos(1,0), T), caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)]).

% caminoConLong
testCaminoOptimo(5) :- tablero(3,2,T), caminoConLong(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)], 3).
testCaminoOptimo(6) :- tablero(3,2,T), not(caminoConLong(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)], 4)).

cantidadTestsCaminoDual(3). 
testCaminoDual(1) :- tablero(3,2,T1), tablero(3,2,T2), caminoDual(pos(0,0), pos(1,1), T1, T2, [pos(0,0), pos(0,1), pos(1,1)]). % Camino correcto
testCaminoDual(2) :- tablero(3,2,T1), tablero(3,2,T2), ocupar(pos(1,0), T1), ocupar(pos(0,1), T2), not(caminoDual(pos(0,0), pos(1,1), T1, T2, _)). % Distintos caminos
testCaminoDual(3) :- tablero(3,2,T1), tablero(3,2,T2), ocupar(pos(1,0), T2), ocupar(pos(0,1), T2), not(caminoDual(pos(0,0), pos(1,1), T1, T2, _)). % T2 no tiene camino

tests(camino2) :- cantidadTestsCamino2(M), forall(between(1,M,N), testCamino2(N)).
tests(caminoAux) :- cantidadTestsCaminoAux(M), forall(between(1, M, N), testCaminoAux(N)).
tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(cantVertices) :- cantidadTestsCantVertices(M), forall(between(1,M,N), testCantVertices(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual),
  tests(camino2),
  tests(cantVertices),
  tests(caminoAux).


tests :- tests(todos).
