data Dict = Empty | Add String Dict Dict

instance Show Dict where
    show Empty = "Empty"
    show (Add k v d) = "Add " ++ show k ++ " (" ++ show v ++ ") (" ++ show d ++ ")"

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

-- RemoveKey: 
removeKey :: String -> Dict -> Dict
removeKey s = recrDict Empty (\k rv rd v d -> if k==s then rd else Add k rv rd)

-- Equals:
equals :: Dict -> Dict -> Bool
equals Empty Empty = True
equals (Add k1 v1 sub1) d2 = def k1 d2 && equals v1 (get k1 d2) && equals sub1 (removeKey k1 d2)
equals _ _ = False