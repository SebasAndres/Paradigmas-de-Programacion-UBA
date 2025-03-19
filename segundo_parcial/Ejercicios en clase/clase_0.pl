natural(cero).
natural(suc(X)) :- natural(X).
menor(cero,suc(X)) :- natural(X).
menor(suc(X),suc(Y)) :- menor(X,Y).

