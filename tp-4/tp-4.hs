-- 1. Pizzas
-- Tenemos los siguientes tipos de datos:
data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente = Salsa
                   | Queso
                   | Jamon
                   | Aceitunas Int

-- EJEMPLOS
pizza1 = Capa Queso(Capa Jamon(Capa (Aceitunas 8)(Capa Queso(Capa Jamon(Capa Queso(Capa Queso(Capa Queso (Capa Jamon Prepizza))))))))
pizza2 = Capa Jamon Prepizza
pizza3 = Capa Queso(Capa Jamon Prepizza)

--Denir las siguientes funciones:


--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza       = 0
cantidadDeCapas (Capa ing piz) = 1 + cantidadDeCapas piz

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza         = Prepizza
sacarJamon (Capa Jamon piz) = sacarJamon piz
sacarJamon (Capa ing piz)   = Capa ing (sacarJamon piz) 

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True 
esJamon _     = False 

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza       = True 
tieneSoloSalsaYQueso (Capa ing piz) = esSalsa ing || esQueso ing && tieneSoloSalsaYQueso piz


esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _     = False 

esQueso :: Ingrediente -> Bool
esQueso Salsa = True
esQueso _     = False 

--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza       = Prepizza
duplicarAceitunas (Capa ing piz) = (Capa duplicarSiEsAceituna ing) (duplicarAceitunas piz) 

duplicarSiEsAceituna :: Ingrediente -> Ingrediente
duplicarSiEsAceituna Aceitunas n = Aceitunas (n * 2)
duplicarSiEsAceituna ing = ing

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:pz) = cantidadCapasDePizza p : cantCapasPorPizza pz

cantidadCapasDePizza :: Pizza -> (Int, Pizza)
cantidadCapasDePizza pizz = (cantidadDeCapas pizz, pizz)
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--2. Mapa de tesoros (con bifurcaciones)
--Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
--cada cofre tiene un objeto, que puede ser chatarra o un tesoro.

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre
                | Bifurcacion Cofre Mapa Mapa


{-
hayTesoro mapa_1 --> True
hayTesoro mapa_2 --> False

-}
{-
                                                Cofre [Chatarra, Chatarra,Chatarra]
                                                 /                             \
                                                /                               \
                            Cofre [Chatarra, Chatarra]                        Cofre [Chatarra, Tesoro]
                                     /             \                                   
                                    /               \                                  
                        Cofre [Chatarra]         Cofre [Chatarra, Tesoro] 

-} 

{-NO TIENE TESOROS
                                                Cofre [Chatarra, Chatarra,Chatarra]
                                                 /                             \
                                                /                               \
                            Cofre [Chatarra, Chatarra]                        Cofre [Chatarra]
                                     /             \                                   
                                    /               \                                  
                        Cofre [Chatarra]         Cofre [Chatarra] 
-}

--Denir las siguientes operaciones:
--Indica si hay un tesoro en alguna parte del mapa.

objetosDeCofre :: Cofre -> [Objeto] -- Observadora de los objetos del cofre 
objetosDeCofre (Cofre obj) = obj 
--
hayTesoro :: Mapa -> Bool
hayTesoro Fin cof = hayTesoroEnObjetos (objetosDeCofre cof)
hayTesoro (Bifurcacion cof m1 m2) = hayTesoroEnObjetos (objetosDeCofre cof) || hayTesoro m1 || hayTesoro m2

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (o:os) = esTesoro o || hayTesoroEnObjetos os

esTesoro :: Objeto ->  Bool
esTesoro Tesoro = True
esTesoro Chatarra = False

--Indica si al nal del camino hay un tesoro. Nota: el nal de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     Fin cof                 = hayTesoroEnObjetos (objetosDeCofre cof)
hayTesoroEn _ Fin cof                      = False
hayTesoroEn [] (Bifurcacion cof _ _)       = hayTesoroEnObjetos (objetosDeCofre cof)
hayTesoroEn (d:ds) (Bifurcacion _ m1 m2)   = if esDirIzq d then hayTesoroEn ds m1 else hayTesoroEn ds m2

esDirIzq :: Dir -> Bool
esDirIzq Izq = True 
esDirIzq _   = False 

--Indica el camino al tesoro. Precondición: existe un tesoro y es único. RECURSION ENTRE FUNCIONES!aaa!!!
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro Fin cof                 = []
caminoAlTesoro (Bifurcacion cof m1 m2) = if hayTesoroEnObjetos (objetosDeCofre cof) 
                                         then [] 
                                         else direccionesATesoroEntre m1 m2

direccionesATesoroEntre :: Mapa -> Mapa -> [Dir]
direccionesATesoroEntre m1 m2 = if hayTesoroEnMapa m1 then izq : caminoAlTesoro m1 else der : caminoAlTesoro m2

--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga Fin cof                 = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = caminoMasLargo 
                                                    (Izq : caminoDeLaRamaMasLarga m1) 
                                                    (Der:caminoDeLaRamaMasLarga m2)

caminoMasLargo :: [Dir] -> [Dir] -> [Dir]
caminoMasLargo xs ys = if length xs >= length ys
                        then xs
                        else ys

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel Fin cof                 = [tesorosEn cof]
tesorosPorNivel (Bifurcacion cof m1 m2) = tesorosEn cof : tesorosPorNivel m1 ++ tesorosPorNivel m2

tesorosEn :: [Objeto] -> [Objeto]
tesorosEn []     = []
tesorosEn (o:os) = if esTesoro o then o: tesorosEn os else tesorosEn os
{-
tesorosPorNivel mapa_1 --> [[],[Tesoro],[Tesoro]]
tesorosPorNivel mapa_2 --> [[],[],[]]
tesorosPorNivel mapa_6 --> [[],[],[Tesoro]]
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ m1 m2) = agregarDir Izq (todosLosCaminos m1) ++ agregarDir Der (todosLosCaminos m2)

agregarDir :: Dir -> [[Dir]] -> [[Dir]]
agregarDir dir [] =  []
agregarDir dir (ds:dss) = (dir : ds) :  agregarDir dir dss

--Nave Espacial
--modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
--dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
--es la siguiente:
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)


nave_1 = N ( NodeT (S "A" [LanzaTorpedos, (Motor 10)] ["Rena", "Gonza"])  (NodeT  (S "B" [LanzaTorpedos, (Motor 20)] ["Azu", "Mile"]) EmptyT 
                                                                                                                                      EmptyT   ) 

                                                                          (NodeT  (S "C" [ (Motor 30)] ["Pabli", "Ami"]) EmptyT
                                                                                                                         (NodeT (S "D" [(Motor 40), Almacen [Comida, Oxigeno, Combustible]] ["Mar"]) EmptyT
                                                                                                                                                                                                     EmptyT )))



{-
                                                S "A" [LanzaTorpedos, (Motor 10)] ["Rena", "Gonza"])  
                                                /                                               \
                                               /                                                 \
        S "B" [LanzaTorpedos, (Motor 20)] ["Azu", "Mile"])                                      S "C" [ (Motor 30)] ["Pabli", "Ami"]
                                                                                                                           \
                                                                                                                            \
                                                                                                                        S "D" [(Motor 40), Almacen [Comida, Oxigeno, Combustible]] ["Mar"]

-}




nave_2 = N ( NodeT (S "E" [LanzaTorpedos, (Motor 1)] [ "Gonza"])  (NodeT  (S "F" [LanzaTorpedos, (Motor 2), Almacen [Oxigeno, Combustible] ] ["Azu"]) EmptyT 
                                                                                                                     EmptyT) 

                                                                  (NodeT  (S "G" [ (Motor 3)] ["Pabli", "Ami"]) EmptyT
                                                                                                                (NodeT (S "H" [Almacen [Comida, Torpedo, Combustible]] ["Mile"]) EmptyT
                                                                                                                                                                                 EmptyT )))




nave_3 = N ( NodeT (S "A" [LanzaTorpedos, (Motor 10)] ["Rena", "Gonza"])  (NodeT  (S "B" [LanzaTorpedos, (Motor 20)] ["Azu", "Mile"]) EmptyT 
                                                                                                                                      EmptyT   ) 

                                                                          (NodeT  (S "C" [ (Motor 30)] ["Pabli","Mile","Ami"]) EmptyT
                                                                                                                         (NodeT (S "D" [(Motor 40), Almacen [Comida, Oxigeno, Combustible]] ["Mar","Rena"]) EmptyT
                                                                                                                                                                                                     EmptyT )))


--Implementar las siguientes funciones utilizando recursión estructural:
--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]


{-
sectores nave_1 --> ["A", "B", "C", "D"]
sectores nave_2 --> ["E", "F", "G", "H"]
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int

{-
poderDePropulsion nave_1  ---> 100
poderDePropulsion nave_2  ---> 6
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]

{-
barriles nave_1 --> [Comida, Oxigeno, Combustible]
barriles nave_2 --> [Oxigeno, Combustible, Comida, Torpedo, Combustible]
-}
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]


{-
tripulantes nave_1 -no tiene repetidos-> ["Rena","Gonza","Azu","Mile","Pabli","Ami","Mar"]
tripulantes nave_3 -tiene repetidos   -> ["Gonza","Azu","Pabli","Mile","Ami","Mar","Rena"]
-}







--Manada de lobos
--Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
--de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
--Los diferentes casos de lobos que forman la jerarquía son los siguientes:
--Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
--Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
--bosques, ríos, etc.), y poseen 2 lobos a cargo.
--Las crías poseen sólo un nombre y no poseen lobos a cargo.
--La estructura es la siguiente:
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cría Nombre

data Manada = M Lobo




manada1 = M (Cazador "A" ["carne","pescado"] (Explorador "B" ["Terr1","Terr2"] (Cría "C") 
                                                                               (Explorador "D" ["Terr3"] (Cría "E") 
                                                                                                         (Cría "F"))) 
                                             (Cría "G") 
                                             (Cazador "H" ["pescado"] (Cría "I")
                                                                      (Cría "J")
                                                                      (Cazador "K" ["pollo", "carne"] (Cría "L")
                                                                                                      (Cría "M")
                                                                                                      (Cría "N"))))
{-                                                                      Cazador "A" ["carne","pescado"]
                                                                       /             |                  \
                                                                      /              |                   \
                                        Explorador "B" ["Terr1","Terr2"]           Cría "G"              Cazador "H" ["pescado"]
                                        /                 \                                             /          |            \
                                       /                   \                                           /           |             \
                                 Cría "C"         Explorador "D" ["Terr3"]                         Cría "I"     Cría "J"       Cazador "K" ["pollo", "carne"]
                                                 /                       \                                                       /          |           \
                                                /                         \                                                     /           |            \       
                                            Cría "E"                   Cría "F"                                             Cría "L"    Cría "M"       Cría "N" 
-}

manada2 = M (Cazador "A" ["carne","pescado","carne","pescado"] (Explorador "B" ["Terr1","Terr2"] (Cría "C") 
                                                                                                 (Explorador "D" ["Terr1"] (Cría "E") 
                                                                                                                           (Cría "F"))) 
                                                               (Cría "G") 
                                                               (Cazador "H" ["pescado","carne","pescado"] (Cría "I")
                                                                                                          (Cría "J")
                                                                                                          (Cazador "K" ["pollo", "carne","carne","pescado"] (Cría "L")
                                                                                                                                                            (Cría "M")
                                                                                                                                                            (Cría "N"))))
{-                                                            Cazador "A" ["carne","pescado","carne","pescado"]
                                                                       /             |                  \
                                                                      /              |                   \
                                        Explorador "B" ["Terr1","Terr2"]           Cría "G"         Cazador "H" ["pescado","carne","pescado"]
                                        /                 \                                             /          |            \
                                       /                   \                                           /           |             \
                                 Cría "C"         Explorador "D" ["Terr1"]                         Cría "I"     Cría "J"    Cazador "K" ["pollo", "carne","carne","pescado"]
                                                 /                       \                                                       /          |           \
                                                /                         \                                                     /           |            \       
                                            Cría "E"                   Cría "F"                                             Cría "L"    Cría "M"       Cría "N"   

          
-}

manada3 = M (Cazador "A" ["carne","pescado","carne","pescado"] (Explorador "B" ["Terr1","Terr2"] (Cría "C") 
                                                                                                 (Explorador "D" ["Terr3"] (Cría "E") 
                                                                                                                           (Cazador "F" ["carne","pescado","carne","pescado","carne","pescado"] (Cría "O")
                                                                                                                                                                                                (Cría "P")
                                                                                                                                                                                                (Cría "Q")))) 
                                                               (Cría "G") 
                                                               (Cazador "H" ["pescado","carne","pescado"] (Cría "I")
                                                                                                          (Cría "J")
                                                                                                          (Cazador "K" ["pollo", "carne","carne","pescado"] (Cría "L")
                                                                                                                                                            (Cría "M")
                                                                                                                                                            (Cría "N"))))
{-                                                             Cazador "A" ["carne","pescado","carne","pescado"]
                                                                       /             |                  \
                                                                      /              |                   \
                                        Explorador "B" ["Terr1","Terr2"]           Cría "G"         Cazador "H" ["pescado","carne","pescado"]
                                        /                 \                                             /          |            \
                                       /                   \                                           /           |             \
                                 Cría "C"         Explorador "D" ["Terr3"]                         Cría "I"     Cría "J"    Cazador "K" ["pollo", "carne","carne","pescado"]
                                                 /                       \                                                                   /          |           \
                                                /                         \                                                                 /           |            \       
                                          Cría "E"       Cazador "F" ["carne","pescado","carne","pescado","carne","pescado"]            Cría "L"    Cría "M"       Cría "N"                
                                                           /                  |                    \                                            
                                                          /                   |                     \                                                 
                                                      Cría "O"             Cría "P"              Cría "Q"




-}



--Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
--crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
--que corresponda en cada caso:

manada = M Cazador "Gonza" ["Pez", "Cierbo", "Conejo"] (Explorador "Azu" ["Rio", "Bosque"] (Cria "Rena") (Cria "Pablo"))
                                                       (Explorador "Ami" ["Lago"]          (Cria "Mar") (Cria "Fidel"))
                                                       (Cria "Mile" )


--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la can-
--tidad de crías.
buenaCaza :: Manada -> Bool



{-
buenaCaza manada1 --> False ( 5 presas y  9 Crías)
buenaCaza manada2 --> True  (11 presas y  9 Crías)
buenaCaza manada3 --> True  (17 presas y 11 Crías)
-}
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas.
elAlfa :: Manada -> (Nombre, Int)


{-
elAlfa manada1 --por como está programado--> ("K",2)
elAlfa manada2 --por como está programado--> ("K",4)
elAlfa manada3 ----------------------------> ("F",6)
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
--pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]


{-
losQueExploraron "Terr1" manada1 --> ["B"]
losQueExploraron "Terr3" manada1 --> ["D"]
losQueExploraron "Terr2" manada1 --> ["B"]
-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un terri-
--torio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]

{-
exploradoresPorTerritorio manada1 --> [("Terr1",["B"]),("Terr2",["B"]),("Terr3",["D"])]   
                         rta real---> [("Terr1",["B"]),("Terr2",["B"]),("Terr3",["D"])]            
exploradoresPorTerritorio manada2 --> [("Terr1",["B","D"]),("Terr2",["B"])] 
                         rta real---> [("Terr2",["B"]),("Terr1",["B","D"])]  -- xq como esta programado lo de sin repetidos me queda al reves
exploradoresPorTerritorio manada3 --> [("Terr1",["B"]),("Terr2",["B"]),("Terr3",["D"])]
                         rta real---> [("Terr1",["B"]),("Terr2",["B"]),("Terr3",["D"])]
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los ca-
--zadores que tienen como subordinado al lobo dado (puede ser un subordinado directo, o el
--subordinado de un subordinado).
cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
--Precondición: hay un lobo con dicho nombre y es único.



--Suponiendo la siguiente manada de ejemplo:
manadaEj = Cazador "DienteFiloso" ["Búfalos", "Antílopes"] (Cría "Hopito") 
                                                           (Explorador "Incansable" ["Oeste hasta el río"] (Cría "MechónGris") 
                                                                                                           (Cría "Rabito") ) 
                                                           (Cazador "Garras" ["Antílopes", "Ciervos"] (Explorador "Zarpado" ["Bosque este"] (Cría "Osado") 
                                                                                                                                            (Cazador "Mandíbulas" ["Cerdos", "Pavos"] (Cría "Desgreñado") 
                                                                                                                                                                                      (Cría "Malcriado") 
                                                                                                                                                                                      (Cazador "TrituraHuesos" ["Conejos"]  (Cría "Peludo") 
                                                                                                                                                                                                                            (Cría "Largo") 
                                                                                                                                                                                                                            (Cría "Menudo") ) ) ) 
                                                                                                      (Cría "Garrita") 
                                                                                                      (Cría "Manchas") )

--la función cazadoresSuperioresDe debería dar lo siguiente:
--cazadoresSuperioresDe "Mandíbulas" manadaEj = ["DienteFiloso", "Garras"]
--cazadoresSuperioresDe "Rabito" manadaEj = ["DienteFiloso"]
--cazadoresSuperioresDe "DienteFiloso" manadaEj = []
--cazadoresSuperioresDe "Peludo" manadaEj = ["DienteFiloso", "Garras", "Mandíbulas", "TrituraHuesos"]


{-
cazadoresSuperioresDe "I" manada3 = ["H", "A"]
cazadoresSuperioresDe "H" manada3 = ["A"]
cazadoresSuperioresDe "E" manada3 = ["A"]
cazadoresSuperioresDe "L" manada3 = ["K","H", "A"]
-}x
