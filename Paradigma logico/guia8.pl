
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).

abuelo(X,Y) :- padre(X,Z), padre(Z,Y).
hermano(X,Y) :- padre(Z,X), padre(Z,Y).
hijo(X,Y) :- padre(Y,X).
descendiente(X,Y) :- padre(Y,X).
descendiente(X,Y) :- padre(Z,X), descendiente(Z,Y).

%% last(?L,?U)
last([X],X).
last([X|LS],U) :- last(LS,U).

%% reverse(+L, -L1)
reverse([],[]).
reverse([X|L1],L2) :- reverse(L1,L3), append(L3,[X],L2).

% prefijo(?P, +L)
prefijo([],_).
prefijo([X|P],[X|L]) :- prefijo(P,L).

% sufijo(?S, +L), donde S es sufijo de la lista L
sufijo(S,[X|S]).
sufijo(S,[X|L]) :- sufijo(S,L).

% sublista(?S, +L), donde S es sublista de L.
% S sufijo de L <= existe un prefijo P de L tal que S es sufijo de P
sublista([],_).
sublista(S,L) :- prefijo(P,L), sufijo(S,P).

%% pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L.
pertenece(X,[X|_]).
pertenece(X,[_|L]) :- pertenece(X,L).

%% o 
pertenece(X,L) :- sublista([X],L).