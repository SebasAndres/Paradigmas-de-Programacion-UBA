desde2(X,X).
desde2(X,Y) :- var(Y), N is X+1, desde2(N,Y).
desde2(X,Y) :- nonvar(Y), Y>X.

% esListaFinitaPositiva([]).
% esListaFinitaPositiva(X) :- desde2(0, Y), sumlist(X,Y).

% generarLista(suma, lista)
generarLista(0,[]).
generarLista(S,[X|XS]) :- 
    S > 0, 
    desde2(1,X), % instancio x >= 1
    S1 is S-X,   % S1 = S-X
    generarLista(S1,XS). % sumlist(XS)=S-X=S1