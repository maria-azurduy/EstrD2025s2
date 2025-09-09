-- 1. Tipos recursivos simples--
-- 1.1. Celdas con bolitas--

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya 
-- existe una operación sobre listas que ayude a resolver el problema

celda1 :: Celda
celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul (Bolita Azul CeldaVacia))))

nroBolitas :: Color-> Celda-> Int
nroBolitas      _    CeldaVacia       = 0
nroBolitas    col1 (Bolita col cel) = unoSiSonDelMismoColor col1 col  + nroBolitas col1 cel

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
poner :: Color-> Celda-> Celda
poner c celda = Bolita c celda

--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar :: Color-> Celda-> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita col cel) =  if esDelMismoColor c col then cel else Bolita col (sacar c cel)

--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda
ponerN n color CeldaVacia = ponerN (n-1) color (Bolita color CeldaVacia)
ponerN n color celda = ponerN (n-1) color (Bolita color celda)

-- 1.2. Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

camino1 :: Camino
camino1 = Cofre [Tesoro,Tesoro] (Cofre [Cacharro] (Nada (Cofre [] Fin)))

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino-> Bool
hayTesoro Fin       = False
hayTesoro (Cofre objs camino) = hayTesoroEnObjetos objs  || hayTesoro camino
hayTesoro (Nada camino)  = hayTesoro camino -- si no hay nada en el camino, me dijo en el siguiente, hasta que haya o llegue al fin.

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (o:os) = esTesoro o || hayTesoroEnObjetos os

esTesoro :: Objeto ->  Bool
esTesoro Tesoro = True
esTesoro Cacharro = False

--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. Si un cofre con un tesoro está al principio del camino,
--la cantidad de pasos a recorrer es 0. Precondición: tiene que haber al menos un tesoro.

pasosHastaTesoro :: Camino-> Int
pasosHastaTesoro Fin       = 0
pasosHastaTesoro (Cofre objs camino) = if hayTesoroEnObjetos objs then 0 else pasosHastaTesoro camino
pasosHastaTesoro (Nada camino)  = 1 +   pasosHastaTesoro camino

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.

hayTesoroEn :: Int-> Camino-> Bool
hayTesoroEn 0 camino = hayTesoroAca camino
hayTesoroEn n Fin       = False
hayTesoroEn n (Cofre objs camino) = hayTesoroEn (n-1) camino
hayTesoroEn n (Nada camino) = hayTesoroEn (n-1) camino


hayTesoroAca :: Camino -> Bool
hayTesoroAca Fin = False
hayTesoroAca (Cofre objs camino) = hayTesoroEnObjetos objs
hayTesoroAca (Nada camino) = False


--Indica si hay al menos "n" tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = n <= cantTesorosEnCamino camino

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin               = 0
cantTesorosEnCamino (Cofre objs camino) = cantTesorosEnLista objs + cantTesorosEnCamino camino
cantTesorosEnCamino (Nada  camino     ) = cantTesorosEnCamino camino

cantTesorosEnLista :: [Objeto] -> Int
cantTesorosEnLista [] = 0
cantTesorosEnLista (o:os) = unoSiEsTesoro o + cantTesorosEnLista os

unoSiEsTesoro :: Objeto -> Int
unoSiEsTesoro Cacharro = 0
unoSiEsTesoro Tesoro   = 1

--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 n camino              = tesorosHastaPaso n camino
cantTesorosEntre x y Fin                 = 0
cantTesorosEntre x y (Cofre objs camino) = cantTesorosEntre (x-1) (y-1) camino
cantTesorosEntre x y (Nada  camino     ) = cantTesorosEntre (x-1) (y-1) camino

tesorosHastaPaso :: Int -> Camino -> Int
tesorosHastaPaso 0 camino              = cantTesorosEnPasoActual camino
tesorosHastaPaso n Fin                 = 0
tesorosHastaPaso n (Cofre objs camino) = cantTesorosEnLista objs + tesorosHastaPaso (n-1) camino
tesorosHastaPaso n (Nada  camino     ) = tesorosHastaPaso (n-1) camino

cantTesorosEnPasoActual :: Camino -> Int
cantTesorosEnPasoActual (Cofre objs camino) = cantTesorosEnLista objs
cantTesorosEnPasoActual       _             = 0


-- 2.1. Árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

t1 = EmptyT
t2 = NodeT 10 EmptyT EmptyT
t3 = NodeT 20
            (NodeT 2 EmptyT EmptyT)
            (NodeT 4 EmptyT EmptyT)
t4 = NodeT 1 (NodeT 2  (NodeT 3 EmptyT EmptyT)
                        (NodeT 4 EmptyT EmptyT))
            (NodeT 4 EmptyT EmptyT)


-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int-> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol 
sizeT :: Tree a-> Int
sizeT EmptyT = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2

-- Dado un árbol de enteros devuelve un árbol con el doble de cada número
mapDobleT :: Tree Int-> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2)  (mapDobleT t1)  (mapDobleT t1)

--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT :: Eq a => a-> Tree a-> Bool 
perteneceT _ EmptyT = False 
perteneceT x (NodeT n t1 t2) = (x==n) || perteneceT x t1  || perteneceT x t2

--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e
aparicionesT :: Eq a => a-> Tree a-> Int 
aparicionesT  _ EmptyT = 0
aparicionesT x (NodeT n t1 t2) = unoSiEsElMismoElemento x n + aparicionesT x t1 + aparicionesT x t2

unoSiEsElMismoElemento :: Eq a => a -> a -> Int
unoSiEsElMismoElemento x y = unoSi (x == y)

-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
-- NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
leaves :: Tree a-> [a]
leaves EmptyT = []
leaves (NodeT x t1 t2) = if (isEmptyT t1 && isEmptyT t2) 
                            then [x] 
                            else leaves t1 ++ leaves t2

isEmptyT :: Tree a->Bool
isEmptyT EmptyT = True
isEmptyT _ = False 

--Dado un árbol devuelve su altura. Nota: altura/height/profundidad, es la cantidad de niveles del árbol1. 
--La altura para EmptyT es 0, y para una hoja es 1.
heightT  :: Tree a-> Int
heightT  EmptyT = 0
heightT  (NodeT x t1 t2)  = 1 + max (heightT t1) (heightT t2)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1))

toList :: Tree a-> [a]
toList EmptyT           = []
toList (NodeT x t1 t2)  = toList t1 ++ [x] ++ toList t2

levelN :: Int-> Tree a-> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

listPerLevel :: Tree a-> [[a]] 
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : unirListasDeNodos (listPerLevel t1) (listPerLevel t2) 

unirListasDeNodos :: [[a]] -> [[a]] -> [[a]]
unirListasDeNodos [] yss = yss
unirListasDeNodos xss [] = xss
unirListasDeNodos (xs:xss) (ys:yss) = (xs ++ ys) : unirListasDeNodos xss yss 

ramaMasLarga :: Tree a-> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : ramaMasLarga (arbolMasGrandeEntre t1 t2) 

arbolMasGrandeEntre :: Tree a-> Tree a-> Tree a
arbolMasGrandeEntre t1 t2 = if (heightT   t1 > heightT   t2) then t1 else t2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1) ++ consACada x (todosLosCaminos t2)

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) =  (x:xs) : consACada x xss

--  2.2. Expresiones Aritméticas

data ExpA = Valor Int
 | Sum ExpA ExpA
 | Prod ExpA ExpA
 | Neg ExpA
 deriving (Show, Eq)

eval :: ExpA-> Int --  Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Valor exp) = exp
eval (Sum exp1  exp2) = eval exp1 + eval exp2
eval (Prod exp1  exp2) = eval exp1 * eval exp2
eval (Neg exp) = - (eval exp)

simplificar :: ExpA -> ExpA
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2)
simplificar (Sum e1 e2) = simplificarSum (simplificar e1) (simplificar e2)
simplificar (Neg exp) = simplificarNeg (simplificar exp)
simplificar exp = exp

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum exp (Valor 0) = exp
simplificarSum (Valor 0) exp = exp
simplificarSum exp expp = Sum exp expp

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd _ (Valor 0) = Valor 0
simplificarProd (Valor 0) _ = Valor 0
simplificarProd exp (Valor 1) = exp
simplificarProd (Valor 1) e = e
simplificarProd exp expp = Prod exp expp

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg exp) = exp
simplificarNeg exp = Neg exp


