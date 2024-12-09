gato(garfield).
tieneMascota(john,odie).
tieneMascota(john,garfield).
amaALosGatos(X) :- tieneMascota(X,Y), gato(Y).
