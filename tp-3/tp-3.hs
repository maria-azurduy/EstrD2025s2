
----- 1 TIPOS RECURSIVOS SIMPLES -----
-- 1.1
-- Celdas con bolitas
-- Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show
{- 
    En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
    de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
    ser la siguiente:
-}
c1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
-- Implementar las siguientes funciones sobre celdas:

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas      _    CeldaVacia       = 0
nroBolitas    color (Bolita col1 cel1) = unoSiSonDelMismoColor color col1  + nroBolitas color cel1

unoSiSonDelMismoColor :: Color -> Color -> Int
unoSiSonDelMismoColor col1 col2 = unoSi (esDelMismoColor col1 col2)

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi x False = []


esDelMismoColor :: Color -> Color -> Bool
esDelMismoColor Azul Azul = True
esDelMismoColor Rojo Rojo = True
esDelMismoColor _ _       = False

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner col cel = Bolita c celda

-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar  _     CeldaVacia      = CeldaVacia
sacar col (Bolita col1 cel1) = if esDelMismoColor col col1 then cel1 else Bolita col (sacar c cel)

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0  _    cel              =  cel
ponerN n col CeldaVacia         =  ponerN (n-1) col (Bolita col CeldaVacia)
ponerN n col cel                =  ponerN (n-1) col (Bolita col cel)

{-
    1.2.
    Camino hacia el tesoro
    Tenemos los siguientes tipos de datos
-}
data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

camino1 :: Camino
camino1 = Cofre [Tesoro,Tesoro] (Cofre [Cacharro] (Nada (Cofre [] Fin)))


-- Definir las siguientes funciones:

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin       = False
hayTesoro (Cofre objs camino) = hayTesoroEnObjetos objs  || hayTesoro camino
hayTesoro (Nada camino)  = hayTesoro camino 

hayTesoroEnObjetos :: [Objeto]  -> Bool
hayTesoroEnObjetos _      = False   
hayTesoroEnObjetos (o:ob) =    esTesoro o || hayTesoroEnObjetos ob

esTesoro :: [Objeto]  -> Bool
esTesoro Cacharro  = True 
esTesoro Tesoro    = True

-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin                 = 0
pasosHastaTesoro (Cofre objs camino) = if hayTesoroEnObjetos objs then 0 else pasosHastaTesoro camino
pasosHastaTesoro (Nada camino)       = 1 + pasosHastaTesoro camino

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0  camino              = hayTesoroAca camino
hayTesoroEn n Fin                  = False 
hayTesoroEn n (Cofre objs camino)  = hayTesoroEn (n-1) camino
hayTesoroEn n (Nada camino)        = hayTesoroEn (n-1) camino

hayTesoroAca :: Camino -> Bool
hayTesoroAca Fin = False
hayTesoroAca (Cofre objs camino) = hayTesoroEnObjetos objs
hayTesoroAca (Nada camino) = False

-- Indica si hay al menos n tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = n => cantTesorosEnCamino camino

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino n Fin                  = 0
cantTesorosEnCamino n (Cofre objs camino)  = 
cantTesorosEnCamino n (Nada camino)        = 

--- DESAFIO ---
-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
-- el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
-- incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre :: Int -> Int -> Camino -> Int

----- 2 TIPO ARBOLES -----

{-
    2.1.
    Árboles binarios
-}
-- Dada esta definición para árboles binarios defina las siguientes funciones utilizando recursión estructural según corresponda:
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show


-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int

-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT :: Tree a -> Int

-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int

-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT :: Eq a => a -> Tree a -> Bool

-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int

-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
-- NOTA: en este tipo se de?ne como hoja a un nodo con dos hijos vacíos
leaves :: Tree a -> [a]

-- Dado un árbol devuelve su altura.
-- Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad de niveles del árbol1. La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int

-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT :: Tree a -> Tree a

-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList :: Tree a -> [a]

-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]

-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]

-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]

{-
    Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos.
    ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o sea, de la raíz hasta la hoja), o sea, por ejemplo:
            todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
                                                EmptyT)
                                     (NodeT 4 (NodeT 5 EmptyT EmptyT)
                                                EmptyT))
            
            = [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
    OBSERVACIÓN: puede resultar interesante plantear otra función, variación de
    ésta para devolver solamente los caminos maximales.
-}
todosLosCaminos :: Tree a -> [[a]]

{-
    2.2.Expresiones Aritméticas
-}
-- El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:

data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA

-- Dada una expresión aritmética devuelve el resultado evaluarla.
eval :: ExpA -> Int

{-
    Dada una expresión aritmética, la simpli?ca según los siguientes criterios (descritos utilizando notación matemática convencional):
    a) 0 + x = x + 0 = x
    b) 0 * x = x * 0 = 0
    c) 1 * x = x * 1 = x
    d) - (- x) = x
-}
simplificar :: ExpA -> ExpA
