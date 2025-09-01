
--  1. RECURSION SOBRE LISTAS
-- Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique lo contrario:
-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = sucesor x : sucesores xs 

sucesor :: Int -> Int
sucesor n = n + 1

-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion []     = False 
conjuncion (b:bs) = esVerdadero b &&  conjuncion bs

esVerdadero :: Bool -> Bool
esVerdadero True = True
esVerdadero _    = False

-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = esVerdadero b || disyuncion bs

-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar []      = []
aplanar (xs:xss) = unirListas xs (aplanar xss)

--
unirListas :: [a] -> [a] -> [a] -- voy a recorrer SOLO la 1ra lista
unirListas   []    ys = ys
unirListas (x:xs)  ys = x : unirListas xs ys
--

-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     =  False
pertenece e (x:xs) =  (e == x) || pertenece e xs

-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = if e == x then 1 + apariciones e xs else apariciones e xs

-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []     =  []
losMenoresA n (x:xs) =  if x < n then x : losMenoresA n xs else losMenoresA n xs

-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []      =  []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n then xs : lasDeLongitudMayorA n xss else lasDeLongitudMayorA n xss

-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista. TRIKYYYYYYY
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     e  = [e]
agregarAlFinal (x:xs) e  = x : agregarAlFinal xs e

-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. Definida en Haskell como (++).
agregar :: [a] -> [a] -> [a]
agregar []      ys = ys
agregar (x:xs)  ys = x : agregar xs ys

-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregarAlFinal (reversa xs)    x
                              -- mi lista    mi elemento

-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.
-- RECORRO DOS ESTRUCTURAS AL MISMO TIEMPO 
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos is []         = is
zipMaximos [] js         = js
zipMaximos (i:is) (j:js) = if i > j then i : zipMaximos is js else j : zipMaximos is js

-- Dada una lista devuelve el mínimo -- obs: la lista NO puede ser vacía
elMinimo :: Ord a => [a] -> a
elMinimo [x]    = x
elMinimo (x:xs) = minimo x (elMinimo xs)

minimo :: Ord a  => a -> a -> a
minimo x y = if x > y then x else y

-- 2. RECURSION SOBRE NUMEROS
-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:
-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial n - 1

--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n < 1 then [] else n : cuentaRegresiva (n - 1)

-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n -1) e 

-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
-- Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ []     = []
losPrimeros 0 _      = [] 
losPrimeros n (x:xs) = x: losPrimeros (n -1) xs 

-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ []     = []
sinLosPrimeros 0 xs     = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs -- cuando llegue al 2do caso, me da la lista completa. lo que pide la funcion 

-- 3. REGISTROS
{-   
    3.1
    Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:
    
-}

data Persona = P String Int -- Nombre Edad 
    deriving (Show, Eq)

persona_1 = P "Amado" 20
persona_2 = P "Beto" 30

--
edad :: Persona-> Int 
edad (P n e) = e
--

--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ []     = []                                                         -- si la lista es vacia no tengo personas que comprobar
mayoresA n (p:ps) = if edad p > n then p: mayoresA n ps else mayoresA n ps    

-- Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad :: [Persona]-> Int 
promedioEdad ps = div (sumatoria (edades ps)) (longitud ps)

edades :: [Persona] -> [Int]                --Dada una lista de personas, devuelve una lista con las edades de esas personas
                                            -- Precondición: la lista al menos posee una persona
edades []       = error "La lista no puede ser vacia"
edades [p]      = [edad p]
edades (p:ps)   = edad p : edades ps

-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo [p]    = p
elMasViejo ps = laQueEsMayor (head ps) (segundo ps) -- recursion  

--aux

-- Dadas dos personas devuelve a la persona que sea mayor
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1@(P _ e1) p2@(P _ e2) = if e1 > e2 then p1 else p2

--
segundo :: [a] -> a
segundo (_:x:_) = x
segundo _       = error "No hay 2 elementos"

--fin aux


{-
    3.2
    Modificaremos la representación de Entreador y Pokemon de la práctica anterior, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
-}

data TipoDePokemon = Agua | Fuego | Planta
    deriving (Show, Eq)
data Pokemon = PK TipoDePokemon Int
    deriving Show
data Entrenador = E String [Pokemon]
    deriving Show

poke_1 = PK Agua 50
poke_2 = PK Fuego 48
poke_3 = PK Agua 32
poke_4 = PK Fuego 32

entrenador_1 = E "A" [poke_3, poke_1, poke_2]
entrenador_2 = E "B" [poke_2, poke_4]

-- Definir en base a esa representación las siguientes funciones:

-- Devuelve la cantidad de Pokémon que posee el entrenador. 1
cantPokemon :: Entrenador -> Int
cantPokemon (E _ ps) = longitud ps

-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador. 2
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (E _ ps) =  cantPokemonDeT tipo ps

cantPokemonDeT :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeT _      []   = 0 
cantPokemonDeT tipo (p:ps) = unoSiEsDeTipo tipo (tipoDe p) + cantPokemonDeT tipo ps

unoSiEsMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEsMismoTipo Agua Agua     = 1
unoSiEsMismoTipo Fuego Fuego   = 1
unoSiEsMismoTipo Planta Planta = 1
unoSiEsMismoTipo    _     _    = 0


--
tipoDe :: Pokemon -> TipoDePokemon -- Devuelve el tipo del pokemon 
tipoDe (PK tp _) = tp 
--

-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tipo (E _ ps1) (E _ ps2) = cuantosDeTipo_En_LeGananA tipo ps1 ps2 

cuantosDeTipo_En_LeGananA :: TipoDePokemon ->  [Pokemon] -> [Pokemon] -> Int
cuantosDeTipo_En_LeGananA tipo []     _   = 0
cuantosDeTipo_En_LeGananA tipo xs     []  = cantPokemonDeT tipo xs 
cuantosDeTipo_En_LeGananA tipo (x:xs) ys  = if tipoDe x == tipo --aca filtro ya el tipo! 
                                            then unoSiLeGana_ATodos x ys + cuantosDeTipo_En_LeGananA tipo xs ys 
                                            else cuantosDeTipo_En_LeGananA tipo xs ys

unoSiLeGana_ATodos :: Pokemon -> [Pokemon] -> Int
unoSiLeGana_ATodos _ []     =  1
unoSiLeGana_ATodos x (y:ys) =  unoSi_SuperaA_ x y * unoSiLeGana_ATodos tipo x ys

unoSi_SuperaA_ :: Pokemon -> Pokemon -> Int
unoSi_SuperaA_ x y (PK tp1 _) (PK tp2 _) = unoSiEsTipoSuperior tp1 tp2 

unoSiEsTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Int
-- Dados dos tipos de Pokemon, devuelve 1 si el primero es superior al segundo
unoSiEsTipoSuperior Agua   Fuego  = 1
unoSiEsTipoSuperior Fuego  Planta = 1
unoSiEsTipoSuperior Planta Agua   = 1
unoSiEsTipoSuperior   _      _    = 0

-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (E _ ps) = hayTodosLosTiposDePokemonesEn ps

hayTodosLosTiposDePokemonesEn :: [Pokemon] -> Bool
hayTodosLosTiposDePokemonesEn [] = False 
hayTodosLosTiposDePokemonesEn ps = hayPokemonDeTipo Agua ps && hayPokemonDeTipo Fuego ps && hayPokemonDeTipo Planta ps

hayPokemonDeTipo ::  TipoDePokemon ->  [Pokemon] -> Bool
hayPokemonDeTipo tipo (p:ps) = tipo == (tipoDe p)  || hayPokemonDeTipo tipo ps
{-
    3.3
    El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
    de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
    una lista de personas con diferente rol. La definición es la siguiente:
-}

data Seniority = Junior | SemiSenior | Senior
    deriving Show

data Proyecto = PR String
    deriving Show

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show

data Empresa = EM [Rol]
    deriving Show


proy1 = PR "Proy A"
proy2 = PR "Proy B"

rol1 = Developer Junior proy1
rol2 = Management Senior proy2

empresa = EM [rol1, rol2]

-- Definir las siguientes funciones sobre el tipo Empresa:

-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (EM rs) = proyectosSinRepetidos rs

proyectosSinRepetidos :: [Rol] -> [Proyecto] 
proyectosSinRepetidos []     = []
proyectosSinRepetidos (r:rs) = agregarProyectoSiNoEstaEn (proyectoDe r) (proyectosSinRepetidos rs)

agregarProyectoSiNoEstaEn :: Proyecto -> [Proyecto] -> [Proyecto]
agregarProyectoSiNoEstaEn p  ps = if proyectoPerteneceALaLista p ps  then ps else p : ps

proyectoDe :: Rol -> Proyecto  -- Dado un rol, devuelve el proyecto
proyectoDe (Developer _ p) = p
proyectoDe (Management _ p) = p

proyectoPerteneceALaLista :: Proyecto -> [Proyecto] -> Bool
proyectoPerteneceALaLista _ [] = False
proyectoPerteneceALaLista x (n:ns) =  (nombreProy x)== (nombreProy n) || (proyectoPerteneceALaLista x ns)

nombreProy :: Proyecto -> String
nombreProy (PR n) = n

-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (EM rs) ps = devSeniorDe_QueTrabajaronEn rs ps 

devSeniorDe_QueTrabajaronEn :: [Rol] -> [Proyecto] -> Int
devSeniorDe_QueTrabajaronEn [] _      = 0
devSeniorDe_QueTrabajaronEn (r:rs) ps = unoSiEsDevSeniorYSuProyectoPerteneceA (seniority r) (proyectoDe r) ps + cantidadDevSeniorYDeProyectos rs ps

unoSiEsDevSeniorYSuProyectoPerteneceA :: Seniority -> Proyecto -> [Proyecto] -> Int
unoSiEsDevSeniorYSuProyectoPerteneceA Senior p ps = unoSiSuProyectoPerteneceA p ps 
unoSiEsDevSeniorYSuProyectoPerteneceA _      _ _  = 0

unoSiSuProyectoPerteneceA :: Proyecto -> [Proyecto] -> Int 
unoSiSuProyectoPerteneceA p ps = if proyectoPerteneceALaLista p ps then 1 else 0

seniority :: Rol -> Seniority 
seniority (Developer s _) = s
seniority (Management s _) = s

-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (EM rs) = cantEmpleadosDe_QueTrabajanEnAlgun ps rs 

cantEmpleadosDe_QueTrabajanEnAlgun :: [Proyecto] -> [Rol] -> Int
cantEmpleadosDe_QueTrabajanEnAlgun _ []      = 0
cantEmpleadosDe_QueTrabajanEnAlgun ps (r:rs) = unoSiTrabajaEnEnAlgunProyecto ps r + cantQueTrabajan ps rs

unoSiTrabajaEnEnAlgunProyecto :: [Proyecto] -> Rol -> Int
unoSiTrabajaEnEnAlgunProyecto ps (Developer _ p)  = unoSiSuProyectoPerteneceA p ps
unoSiTrabajaEnEnAlgunProyecto ps (Management _ p) = unoSiSuProyectoPerteneceA p ps

-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (EM rs) =  cantPorProyecto rs

cantPorProyecto :: [Rol] -> [(Proyecto, Int)]
cantPorProyecto []      = []
cantPorProyecto (r:rs)  = agregarATupla r (cantPorProyecto rs)

agregarATupla :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)] 
agregarATupla (Developer _ p)  ps = sumarUnoEnTuplaCorrespondiente p ps
agregarATupla (Management _ p) ps = sumarUnoEnTuplaCorrespondiente p ps


sumarUnoEnTuplaCorrespondiente :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)] 
sumarUnoEnTuplaCorrespondiente p   []   = [(p, 1)] -- si llega a esto es xq la tupla no existe, por lo que tiene que crearla de cero
sumarUnoEnTuplaCorrespondiente p (t:ts) = if esLaTupla p t 
                                            then sumarUnoA t : ts 
                                            else t : sumarUnoEnTuplaCorrespondiente p ts

esLaTupla :: Proyecto -> (Proyecto, Int) -> Bool
esLaTupla p1 (p2, _) = nombreProy p1  == nombreProy p2

sumarUnoA :: (Proyecto, Int) -> (Proyecto, Int) 
sumarUnoA (p , n) = (p, n+1)


