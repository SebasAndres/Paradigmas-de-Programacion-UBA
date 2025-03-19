# Final Diciembre 2024

## 1. Esquemas de recursion

Definir recursión primitiva, recursión estructural en base a la recursión primitiva.
Después definir una función Split que tome un Dato a b y devuelva dos listas, una con los a y otra con los b, usando el fold o rec.

Se da una estructura 
~~~hs
data Dato a b = 
    C1 |                    
    C2 a |                  
    C3 b (Dato a b) (Dato a b). 

recrDato :: c                         -- para el caso C1
         -> (a -> c)                  -- para el caso C2
         -> (b -> c -> c -> Dato a b -> Dato a b -> c)  -- para el caso C3
         -> Dato a b 
         -> c
recrDato cC1 cC2 cC3 d = case d of 
    C1 -> cC1
    C2 x -> cC2 x
    C3 y d1 d2 -> cC3 y (rec d1) (rec d2) d1 d2
    where rec = recrDato cC1 cC2 cC3

foldDato :: c                         -- para el caso C1
         -> (a -> c)                  -- para el caso C2
         -> (b -> c -> c -> c)  -- para el caso C3
         -> Dato a b 
         -> c
foldDato c1 c2 c3 = recrDato c1 c2 (\y r1 r2 d1 d2 -> c3 y r1 r2) 

split :: Dato a b -> ([a], [b])
split d = foldDato 
    (const [])
    (\x -> [[x], []])
    (\y (ar1, ar2) (br1, br2) -> [ar1++br1, y : ar2++br2])
~~~

## 3. Inferencia

Te daba la regla de inferencia para la aplicación. Después daba dos variantes y pedía explicar por qué no funcionaban. Una quitaba la condición de que los contextos de tipado de los términos tenían que unificar para todas las variables compartidas, el otro en vez de generar una incógnita fresca le asignaba una variable de tipo directamente a la aplicación. La idea era dar una inferencia de tipo errónea con cada regla.

## 4. Resolución
~~~pl
p(a).
p(b).
q(a).
q(c).
r(X,Y) :- q(X), !, p(Y).
s(X,Y) :- p(X), not(r(X,Y)).
s(Y,Y) :- q(c), r(c,Y).
~~~

Dar el arbol de búsqueda para la consulta S(X,Y).

~~~
S(X,Y)
|-> p(X), not(r(X,Y))
|    |-> not(r(a,Y)) .............................. {X = a}
|    |    |-> r(a,Y), !, Fail
|    |    |    |-> q(a), !, p(Y), !, Fail
|    |    |        |-> !, p(Y), !, Fail
|    |    |           |-> p(Y), !, Fail
|    |    |           |   |-> !, Fail, {Y = a}
|    |    |           |        |-> !, Fail
|    |    |           |            |-> Fail.
|    |    |           |               |-> X
|    |    |           |-> cut
|    |    |-> cut
|    |-> not(r(b,Y)) ----------------------- {X = b}
|        |-> r(b, Y), !, Fail
|            |-> q(b), !, p(Y)
|                |-> X
|-> q(c), r(c, Y).
    |-> r(c, Y)
        |-> q(c), !, p(Y)
            |-> !, p(Y)
                |-> p(Y)
                    |-> V --------------- {Y = a}
                    |-> V --------------- {Y = b}
~~~

## 5. Prolog
Tomando una representación de grafos como lista de aristas en Prolog (ejemplo: [(a,b), (b, c), (d,c)]). Se pide definir los siguientes predicados:
- [a] caminosSimples(+G, +X, +Y, -P), que pide devolver todos los caminos simples entre X e Y. G se puede asumir acíclico.
~~~pl
%% Version G&T
caminoSimpleGT(G, X, Y, CS) :- 
    nodos(G, L) % Obtiene los nodos de G
    generarCamino(L, X, Y, C), % Genera todos los caminos con nodos de L que van de X a Y 
    perteneceAlGrafo(C, G), % Valida que el camino pertenezca al grafo 
    member(C, CS).  % Pide que ese camino este en la lista a instanciar

%% Hago otra porque no me parece muy eficiente el anterior
caminosSimples(G, X, Y, P) :- caminosSimplesAuxiliar(G, G, X, Y, P). 

% caminosSimplesAuxiliar(+G, +R, +X, +Y, -P). 
% Recibe el grafo dos veces porque no queremos perder informacion a medida que recorremos le grafo
% R es la parte de la lista de aristas que queda por validar y G el grafo completo
caminosSimplesAuxiliar(_, [], _, _, []).
caminosSimplesAuxiliar(G, [(X,Y)|Rss], X, Y, [[(X,Y)]|Pss]) :- caminosSimplesAuxiliar(G, Rss, X, Y, Pss).
caminosSimplesAuxiliar(G, [(X,B)|Rss], X, Y, [P1 | Pss]) :- 
    hayCaminoHasta(G, X, Y),
    generarCamino(P1, X, Y), 
    todasLasAristasEstanEnElGrafo(P1, G),
    caminosSimplesAuxiliar(G, Rss, X, Y, Pss).

% esCamino(+L, +X, +Y)
esCamino([(X, Y)], X, Y).
esCamino([(X, A) | Ps], X, Y) :- esCamino(Ps, A, Y).

% todasLasAristasEnElGrafo(+E, +L)
todasLasAristasEstanEnElGrafo([], []).
todasLasAristasEstanEnElGrafo([E|EX], L) :- member(E, L), todasLasAristasEstanEnElGrafo(EX, L).


%%% GPT:
% Caso base: Si estamos en Y, el único camino es [Y].
caminoSimple(G, X, Y, [X, Y]) :- edge(X, Y, G).

% Caso recursivo: Avanzar por el grafo, asegurando que no hay ciclos.
caminoSimple(G, X, Y, [X | Camino]) :-
    edge(X, Z, G),       % Tomamos un vecino Z de X
    Z \= Y,              % No queremos ir directamente a Y (ese caso está arriba)
    \+ member(Z, Camino), % Evitamos ciclos verificando que no esté en el camino previo
    caminoSimple(G, Z, Y, Camino).

% Predicado auxiliar para verificar si hay una arista entre dos nodos.
edge(A, B, G) :- member((A, B), G).
edge(A, B, G) :- member((B, A), G). % Si las aristas son bidireccionales

~~~

- [b] aciclico(+G), que devuelve si G es aciclico o no.
~~~pl
% aciclico(+G), que devuelve si G es aciclico o no.
% Un grafo es acíclico si no hay algun camino que pueda empezar en un nodo y termine en el mismo.
aciclico(G) :- not(nodo(G, N), caminosSimple(G, N, N, P), length(P, M), M > 0).
~~~

- [c] caminos(+G, +S, +E, -P), que devuelve todos los caminos entre S y E, para un grafo que puede tener cíclicos.

~~~pl
% caminos(+G, +S, +E, -P), que devuelve todos los caminos entre S y E, para un grafo que puede tener cíclicos.

~~~

Finalmente había que analizar la reversibilidad sobre G y S para este último.

## 6. Probar con deducción natural

$$∀X(P(X)∨Q(X)) → ¬(∃Z(¬P(Z)∧¬Q(Z)))$$

Después daba una fórmula en cálculo lambda que era una función sobre unión disjunta (o sea, un término de tipo σ + τ) y pedía dar la fórmula lógica que correspondería a dicho término y justificar por qué correspondía (correspondencia Curry Howard).