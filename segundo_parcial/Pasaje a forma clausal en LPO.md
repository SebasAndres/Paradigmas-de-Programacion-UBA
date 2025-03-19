# Pasaje a forma clausal en LPO.

1. Reemplazar los $(a \to b)$ con $(\neg a \lor b)$.
2. Empujar el $\neg$ hacia adentro (tras esto obtenemos la forma normal negada).
    - $\neg (\forall X. \sigma) \equiv \exists X. \neg \sigma$
    - $\neg (\exists X. \sigma) \equiv \forall X. \neg \sigma$
3. Extraer los cuantificadores hacia afuera asumiendo $X\notin fv(\tau)$ (tras esto obtenemos la forma normal prenexa).
    - i.e. $(∀X. σ) ∧ τ \equiv ∀X.(σ ∧ τ ) $
4. Skolemnizacion :: Deshacerse de los cuantificadores existenciales usando la tecnica de Herbrand-Skolem (Y en funcion de las variables ligadas antes por cuantificadores, si no hay, constante).

$$\forall X_1..X_n \exists Y P(X,Y) \equiv \forall X_1..X_n P(X,f(X_1...X_n))$$
$$\exists Y P(Y) \equiv P(c)$$

5. Pasaje a forma normal conjuntiva (la disjuncion de uniones). Por ejemplo: 
$$\sigma = (a \lor b) \land (c \lor d)$$

6. Empujar los cuantificadores universales hacia el centro.

### Resumen
1. Reescribir ⇒ usando ¬ y ∨.
2. Pasar a f.n. negada, empujando ¬ hacia adentro.
3. Pasar a f.n. prenexa, extrayendo ∀, ∃ hacia afuera.
4. Pasar a f.n. de Skolem, Skolemizando los existenciales.
5. Pasar a f.n. conjuntiva, distribuyendo ∨ sobre ∧.
6. Empujar los cuantificadores hacia adentro de las conjunciones.
