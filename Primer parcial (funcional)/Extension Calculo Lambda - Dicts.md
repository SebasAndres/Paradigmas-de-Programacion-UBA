
# Extension de calculo $\lambda$ para diccionarios

No está validado!!
> Faltan las reglas de tipado

## Tipos
$$t := ... | dicc(t,t)$$

## Términos
$$M := ... | \{\}_{\sigma,\tau} | add(M,M,M) | \text{case M of } \{\}_{\sigma,\tau} \rightarrow M; add(M,M,M) \rightarrow M | \text{def?(M,M) | get(M,M) | equals(M,M) | removeKey(M,M)}$$

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

$removeKey(K, \{\}_{\sigma,\tau}) \rightarrow \{\}_{\sigma,\tau}$.

$removeKey(K, Add(K',V',D')) \rightarrow \text{if K=K' then D' else Add(K',V',removeKey(K,D'))}$.


## Reglas de congruencia
- 3 para $\text{add(M,M,M)}$
- 2 para $\text{def?(M,M)}$
- 2 para $\text{get(M,M)}$
- 1 para $\text{case M of } \{\}_{\sigma,\tau} \rightarrow M; add(M,M,M) \rightarrow M $
- 2 para $\text{equals(M,M)}$
- 2 para $\text{removeKey(M,M)}$

## Extra. Implementación de Dict en Haskell
~~~haskell
-- Estructura
data Dict = Empty | Add String Dict Dict

-- Para visualizarlo
instance Show Dict where
    show Empty = "Empty"
    show (Add k v d) = "Add " ++ show k ++ " (" ++ show v ++ ") (" ++ show d ++ ")"

-- Esquemas de recursión
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

-- Cómputo de términos
def :: String -> Dict -> Bool
def s = foldDict False (\k rv rd -> s == k || rd)  -- solo claves del primer nivel
 
get :: String -> Dict -> Dict
get s = recrDict Empty (\k rv rd v d -> if s==k then v else rd)

removeKey :: String -> Dict -> Dict
removeKey s = recrDict Empty (\k rv rd v d -> if k==s then rd else Add k rv rd)

equals :: Dict -> Dict -> Bool
equals Empty Empty = True
equals (Add k1 v1 sub1) d2 = def k1 d2 && equals v1 (get k1 d2) && equals sub1 (removeKey k1 d2)
equals _ _ = False
~~~
