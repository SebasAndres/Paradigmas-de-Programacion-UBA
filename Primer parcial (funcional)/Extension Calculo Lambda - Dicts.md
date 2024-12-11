
# Extension de calculo $\lambda$ para diccionarios

No está validado!!
> Faltan las reglas de tipado

## Tipos
$$t := ... | dicc(t,t)$$

## Términos
$$M := ... | \{\}_{\sigma,\tau} | add(M,M,M) | \text{case M of } \{\}_{\sigma,\tau} \rightarrow M; add(M,M,M) \rightarrow M | \text{def?(M,M) | get(M,M) | equals(M,M)}$$

## Valores
$$V := ... | \{\}_{\sigma,\tau} | add(V,V,V)$$


## Semántica operacional

$(\text{case } \{\}_{\sigma,\tau} \text{ of } \{\}_{\sigma,\tau} \rightarrow N; add(K,V,D) \rightarrow O) \rightarrow N$.

$(\text{case add(K,V,D) of } \{\}_{\sigma,\tau} \rightarrow N; add(K',V',D') \rightarrow O) \rightarrow O$.

$def?(\{\}_{\sigma,\tau}, R) \rightarrow false$.

$def?(add(K,V,D),W) \rightarrow \text{if K=W then true else def?(D,W)}$.

$get(M,\{\}_{\sigma,\tau}) \rightarrow get(M,\{\}_{\sigma,\tau})$. // se cuelga intencionalmente

$get(M,add(K,V,D)) \rightarrow \text{if K=M then V else get(M,D)}$.

$equals(\{\}_{\sigma,\tau},N) \rightarrow (\text{case N of } \{\}_{\sigma,\tau} \rightarrow \text{ true; Add \_ \_ \_ } \rightarrow false)$.

$equals(Add(K,V,D),N) \rightarrow (\text{case N of } \{\}_{\sigma,\tau} \rightarrow \text{ false; Add K' V' D' } \rightarrow \text{K'=K \&\& equals(V,V') \&\& equals(D,D')})$.

## Reglas de congruencia
- 3 para $\text{add(M,M,M)}$
- 2 para $\text{def?(M,M)}$
- 2 para $\text{get(M,M)}$
- 1 para $\text{case M of } \{\}_{\sigma,\tau} \rightarrow M; add(M,M,M) \rightarrow M $
- 2 para $\text{equals(M,M)}$

## Extra. Implementación de Dict en Haskell
~~~haskell
data Dict = Empty | Add String Dict Dict

foldDict :: a -> (String -> a -> a -> a) -> Dict -> a
foldDict cEmpty cAdd t = case t of 
    Empty -> cEmpty
    Add k v d -> cAdd k (recu v) (recu d)
    where recu = foldDict cEmpty cAdd

recrDict :: a -> (String -> a -> a -> Dict -> Dict -> a) -> Dict -> a
recrDict cEmpty cAdd t = case t of 
    Empty -> cEmpty
    Add k v d -> cAdd k (recu v) (recu d) v d
    where recu = recrDict cEmpty cAdd

-- Def: solo busca en en las claves del primer nivel
def :: String -> Dict -> Bool
def s = foldDict False (\k rv rd -> s == k || rd) 

-- Get: asume que s es una llave en el diccionario pasado
get :: String -> Dict -> Dict
get s = recrDict Empty (\k rv rd v d -> if s==k then v else rd)

-- Equals
equals :: Dict -> Dict -> Bool
equals d1 d2 = recrDict 
    (case d2 of Empty -> True; Add _ _ _ -> False)  -- cEmpty
    (\k1 rv rd v1 sub1 ->                           -- cAdd 
        case d2 of
            Empty -> False
            Add k2 v2 sub2 -> k1 == k2 && equals v1 v2 && equals sub1 sub2
    ) 
    d1

-- Pd: Usando pattern-matching sale + facil
equalsPM :: Dict -> Dict -> Bool
equalsPM Empty Empty = True
equalsPM (Add s1 v1 d1) (Add s2 v2 d2) = s1 == s2 && equalsPM v1 v2 && equalsPM d1 d2
equalsPM _ _ = False

~~~
