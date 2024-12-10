En lógica formal, decir que un **secuente** es válido significa que hay una **prueba formal** que demuestra que, dadas las hipótesis (premisas) \(\sigma_1, \sigma_2, \ldots, \sigma_n\), se puede deducir la conclusión \(\tau\). Vamos a desglosar esto con más detalle.

---

### 🔍 **Concepto de secuente**

Un **secuente** es una expresión de la forma:

\[
\sigma_1, \sigma_2, \ldots, \sigma_n \vdash \tau
\]

Esto se lee como: *"Bajo las hipótesis \(\sigma_1, \sigma_2, \ldots, \sigma_n\), se puede deducir la conclusión \(\tau\)"*.

---

### 📌 **¿Qué significa que haya una prueba?**

Que haya una **prueba** de \(\tau\) a partir de las hipótesis \(\sigma_1, \sigma_2, \ldots, \sigma_n\) significa que existe una secuencia finita de pasos (aplicaciones de reglas de inferencia) que parte de las hipótesis y termina en \(\tau\). En otras palabras:

1. **Se parte de las hipótesis**: \(\sigma_1, \sigma_2, \ldots, \sigma_n\).
2. **Se aplican reglas de inferencia válidas**: Cada paso de la prueba se justifica mediante una regla de inferencia del sistema lógico en el que trabajas.
3. **Se llega a la conclusión** \(\tau\).

---

### 📜 **Ejemplo de prueba en lógica proposicional**

Consideremos el secuente:

\[
P, P \to Q \vdash Q
\]

Esto significa: *"Si tenemos \(P\) y \(P \to Q\) como hipótesis, podemos deducir \(Q\)"*. Una prueba formal de esto puede ser:

1. **Hipótesis**: \(P\)
2. **Hipótesis**: \(P \to Q\)
3. **Aplicación de Modus Ponens** (de 1 y 2): \(Q\)

Por lo tanto, hay una prueba de \(Q\) a partir de las hipótesis \(P\) y \(P \to Q\).

---

### 🧩 **Características de una prueba**

1. **Finitud**: Una prueba tiene una cantidad finita de pasos.
2. **Reglas de inferencia**: Cada paso sigue una regla de inferencia válida.
3. **Deducción válida**: Si todas las premisas son verdaderas, la conclusión debe ser verdadera.

---

### ✅ **Validez del secuente**

Un secuente \(\sigma_1, \sigma_2, \ldots, \sigma_n \vdash \tau\) es válido si **hay una prueba** de \(\tau\) usando únicamente las hipótesis \(\sigma_1, \sigma_2, \ldots, \sigma_n\) y las reglas del sistema lógico. En otras palabras, la conclusión \(\tau\) se deduce lógicamente de las hipótesis.

---

### 💡 **Resumen**

- Un **secuente** es válido si hay una **prueba formal** que deduce la conclusión a partir de las hipótesis.
- Una **prueba** es una secuencia finita de aplicaciones de reglas de inferencia válidas que llevan de las hipótesis a la conclusión.
- La **validez del secuente** garantiza que si las hipótesis son verdaderas, la conclusión también lo será.