
module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = M [(k,v)]
    deriving Show

--Propósito: devuelve un map vacío
emptyM :: Map k v -- --> O(1)
emptyM = M []


--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v -- tiene costo O(n), donde n es la cantidad de elementos en el map
assocM k v (M ks) = M (asociar k v ks)

asociar :: Eq k => k -> v -> [(k,v)] -> [(k, v)]
asociar k v []       = [(k,v)]             --dejas la clave y le pones el nuevo valor 
asociar k v ((k',v'):kvs) = if k == k' then (k',v):kvs else (k',v') : asociar k v kvs
--OBS: en caso de que ya exista una tupla con esa clave, la pisa con la nueva. En caso de que no exista la agrega al final


--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = lookupM' k kvs 

lookupM' :: Eq k => k -> [(k,v)] -> Maybe v
lookupM' k [] = Nothing
lookupM' k ((k',v'):kvs) = if k == k' then Just v' else lookupM' k kvs 

--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M ks) = M (borrar k ks)

borrar :: Eq k => k -> [(k,v)] -> [(k, v)]
borrar k [] = []
borrar k ((k',v'):kvs) = if k == k' then kvs else (k',v') : borrar k kvs 

--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (M kvs) = clavesDe kvs

clavesDe :: [(k,v)] -> [k]
clavesDe []            = []
clavesDe ((k,v):kvs) = k : clavesDe kvs 

----------------------------------------------------------USUARIO----------------------------------------------------------------------------

--Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v-> [Maybe v]
valuesM map = valoresDe map (keys map)

valoresDe :: Eq k => Map k v-> [k] -> [Maybe v] -- necesito el mapa xq para usar lookup necesito el mapa 
valoresDe _ []       = [] 
valoresDe map (k:ks) = lookupM k map : valoresDe map ks 

--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k]-> Map k v-> Bool
todasAsociadas [] map = True --si llegue al caso base, ya recorri todas las claves y todas pertenecieron
todasAsociadas (k:ks) map = elem k (keys map) && todasAsociadas ks map --si deja de cumplirse, corta y es False 

--Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)]-> Map k v
listToMap [] = emptyM
listToMap ((k,v):kvs) =  assocM k v (listToMap kvs)         --preguntar sobre abrir o no la estruc

--Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v-> [(k, v)]
mapToList map = mapToList' map (keys map)

mapToList' :: Eq k => Map k v-> [k] -> [(k, v)] 
mapToList' map [] = []
mapToList' map (k:ks) = case lookupM k map of
                            Just v -> (k, v) : mapToList' map ks
                            Nothing -> mapToList' map ks

--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k, v)]-> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,v):kvs) = let par = (k,v) 
                        in agruparEq' k v (agruparEq kvs)

agruparEq' :: Eq k => k -> v -> Map k [v] -> Map k [v]
agruparEq' k v map = case lookupM k map of
                            Just vs -> assocM k (v:vs) map
                            Nothing -> assocM k [v] map

--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar :: Eq k => [k]-> Map k Int-> Map k Int
incrementar [] map = map
incrementar (k:kv) map = case lookupM k map of
                            Just n -> assocM k (n+1) (incrementar kv map)
                            Nothing -> incrementar kv map

--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = mergeMaps' (mapToList m1) m2

mergeMaps' :: Eq k => [(k,v)] -> Map k v -> Map k v
mergeMaps' [] map     = map
mergeMaps' (k:ks) map = let (c,b) = k -- preguntar el let aca
                        in assocM c b (mergeMaps' ks map)

indexar :: [a] -> Map Int a
indexar xs = indexar' 0 xs

indexar' :: Int -> [a] -> Map Int a
indexar' n []     = emptyM 
indexar' n (x:xs) = assocM n x (indexar' (n+1) xs)

