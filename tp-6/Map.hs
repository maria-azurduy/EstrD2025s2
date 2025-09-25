{-
La interfaz del tipo abstracto Map es la siguiente:

1. Como una lista de pares-clave valor sin claves repetidas
-}

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

asociar :: Eq k => k -> v -> [(k,v)] 
asociar k v []       = [(k,v)]
asociar k v (xs:xss) = 

agregarALaLista :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
--OBS: en caso de que ya exista una tupla con esa clave, la pisa con la nueva. En caso de que no exista la agrega 
-- al final


--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v


--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v



--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]

