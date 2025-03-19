# Sustitucion y Unificacion [14/6/24]
...

Dado un programa logico P y n goal G_1 ... G_n se quiere saber si el goal es consecuencia logica de P.
Usando la siguiente regla de reduccion

$$\frac{G1,...,G_i,...,G_n\hspace{0.3cm} H:- A_1,...,A_k \hspace{0.3cm}\text{ donde }\sigma \text{ es el MGU de Gi y H}}{\sigma(G_1,...,G_{i-1},A_1,...,A_k,G_{i+1}, ..., G_n)}$$
