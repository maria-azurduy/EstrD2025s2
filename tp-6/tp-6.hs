module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ, heapSort)
where

{-
Ejercicio 1
La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
Implementarla usando listas, e indicando el costo de cada operación.
-}

data PriorityQueue a = Pq [a]
    deriving Show

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a -- O(1)
emptyPQ = Pq []


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool -- O(1)
isEmptyPQ (Pq xs) = null xs

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: inserta un elemento en la priority queue. 
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (Pq xs) = Pq (x:xs)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía. --> La priority que no está vacía
findMinPQ :: Ord a => PriorityQueue a -> a -- O(1)
findMinPQ (Pq xs) = head xs 


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.  --> La priority que no está vacía
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(1)
deleteMinPQ (Pq xs) = Pq (tail xs)








-- dada una lista la ordena de menor a mayor utilizando una PriorityQueue como estructura auxiliar.
heapSort :: Ord a => [a]-> [a]
heapSort xs = heapSorts (ingresarAPq xs)


--Agrego cada elemento de la lista a una PQ 
ingresarAPq :: Ord a => [a] -> PriorityQueue a 
ingresarAPq []      = emptyPQ
ingresarAPq (x:xs)  = insertPQ x (ingresarAPq xs)

--Organiza la PQ y devuelve la lista ya ordenada
heapSorts :: Ord a => PriorityQueue a -> [a]
heapSorts pq = if isEmptyPQ pq 
                then []
                else findMinPQ pq : heapSorts (deleteMinPQ pq)

--


{-

1. Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los
ordenes de complejidad en peor caso de cada función de la interfaz, justificando las respuestas.

-}

module MultiSet 
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) 
where

data MultiSet a = MS (Map a Int)
    deriving Show

--Propósito: denota un multiconjunto vacío.
emptyMS :: MultiSet a
emptyMS = MS (emptyM)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
--multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MS m) = case lookupM x m of 
                    Just v -> MS (assocM x (v+1) m)
                    Nothing -> MS (assocM x 1 m)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (MS m) = case lookupM x m of 
                    Just v -> v
                    Nothing -> 0



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
--ambos multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)
unionMS (MS m1) (MS m2) = let ks = keys m1 ++ keys m2 --tengo una lista de claves de ambos maps 
                            in listToMS ks 
                            where 
                                listToMS []     = emptyMS
                                listToMS (x:xs) = let v = ocurrencesMS x (MS m1) + ocurrencesMS (MS m2)
                                                    in if v == 0 
                                                        then listToMS ks
                                                        else MS (assocM x v (case listToMS ks of 
                                                                                    MS m -> m))
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
--multiconjuntos tienen en común.

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)
intersectionMS (MS m1) (MS m2) = let ks = keys m1
                                  in listToMS ks
                                    where
                                        listToMS []     = emptyMS
                                        listToMS (k:ks) = let n = min (ocurrencesMS k (MS m1)) (ocurrencesMS k (MS m2))
                                                            in if n == 0
                                                                then listToMS ks
                                                                else MS (assocM k n (case listToMS ks of MS m -> m))


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
multiSetToList (MS m) = expand (keys m)
                        where
                        expand []     = []
                        expand (k:ks) = copies (ocurrencesMS k (MS m)) k ++ expand ks

                        copies 0 _ = []
                        copies n x = x : copies (n-1) x




--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


ms1 :: MultiSet String
ms1 = MS m1

ms2 :: MultiSet String
ms2 = MS m2

m1 = assocM "a" 1 (assocM "b" 2 (assocM "c" 3 (assocM "d" 4 emptyM)))

m2 = assocM "a" 1 (assocM "f" 2 (assocM "c" 3 (assocM "h" 4 (assocM "i" 5 (assocM "j" 6 emptyM)))))


{-
addMS "e" ms1 ==> MS (M [("d",4),("c",3),("b",2),("a",1),("e",1)])
addMS "a" ms1 ==> MS (M [("d",4),("c",3),("b",2),("a",2)])

ocurrencesMS "a" ms1 ===> 1
ocurrencesMS "c" ms1 ===> 3

unionMS ms1 ms2 ==> MS (M [("d",4),("c",6),("b",2),("a",2),("f",2),("h",4),("i",5),("j",6)])


intersectionMS ms1 ms2 ==> MS (M [("c",6),("a",2)]) -- Tome como que tengo q sumar los valores de las claves q coinciden en ambos MS

multiSetToList ms1 ==> [("d",4),("c",3),("b",2),("a",1)]

-}

