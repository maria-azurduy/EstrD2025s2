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