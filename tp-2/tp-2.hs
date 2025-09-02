sumatoria :: [Int]-> Int --  Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria []     = 0 --mi caso base
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a]-> Int -- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int]-> [Int] -- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero. 
sucesores [] = []
sucesores (n:ns) =  n+1 :  sucesores ns

conjuncion :: [Bool]-> Bool -- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion [] = True
conjuncion (b:bs) = b && (conjuncion bs)


disyuncion :: [Bool]-> Bool -- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion [] = False
disyuncion (b:bs) = b || (disyuncion bs)


aplanar :: [[a]]-> [a] -- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar [] = []
aplanar (xs:xss) = unirListas xs (aplanar xss)

--
unirListas :: [a] -> [a] -> [a] -- voy a recorrer la 1ra lista
unirListas   []    ys = ys
unirListas (x:xs)  ys = x : unirListas xs ys
--

pertenece :: Eq a => a -> [a] -> Bool --  Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs


apariciones :: Eq a => a-> [a]-> Int -- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones e [] = 0
apariciones e (x:xs) = if x == e
                        then 1 + apariciones e xs
                        else apariciones e xs

{- 
apariciones e [] = 0
apariciones e (x:_) = 1 + apariciones e xs
apariciones e (_:xs) = apariciones e xs

-}

losMenoresA :: Int-> [Int]-> [Int] -- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA k [] = []
losMenoresA k (n:ns) = if k > n
                        then n : losMenoresA k ns
                        else losMenoresA k ns

lasDeLongitudMayorA :: Int-> [[a]]-> [[a]] --  Dados un número n y una lista de listas, devuelve la lista de aquellas 
                                           --listas que tienen más de n elementos.
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n
                                    then xs : lasDeLongitudMayorA n xss
                                    else lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a] --Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e =   x :  agregarAlFinal xs e


agregar :: [a]-> [a]-> [a] -- Dadas dos listas devuelve la lista con todos los elementos de la primera lista +  todos los elementos de la segunda a continuación.
agregar []      ys = ys
agregar (x:xs)  ys = x : agregar xs ys


reversa :: [a] -> [a] --Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa []     = []
reversa (x:xs) = agregarAlFinal (reversa xs) x


zipMaximos :: [Int] -> [Int] -> [Int] --Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
                                        --máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
                                            --las listas no necesariamente tienen la misma longitud.
zipMaximos []  ys = ys
zipMaximos xs  [] = xs
zipMaximos (x:xs) (y:ys) = if x>y then x : zipMaximos xs ys
                                    else y : zipMaximos xs ys

elMinimo :: Ord a => [a] -> a  --Dada una lista devuelve el mínimo
                               -- obs: la lista NO puede ser vacía
elMinimo [x] = x
elMinimo (x:xs) = minimo x (elMinimo xs)

minimo :: Ord a => a -> a -> a
minimo x y =  if x < y 
                then x 
                else y

--  2. Recursión sobre números -- 

factorial :: Int-> Int --Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. 
                        --Si n es 0 devuelve 1. 
                        --La función es parcial si n es negativo.
                        --Pre: n no puede ser negativo

factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int-> [Int]  --Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
                                --n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva n = if n<1 then []
                    else n : cuentaRegresiva (n-1)

repetir :: Int-> a-> [a] --Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir 0 _ = []
repetir n x = x : repetir (n-1) x

losPrimeros :: Int-> [a]-> [a] --Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
                               --Si la lista es vacía, devuelve una lista vacía.
losPrimeros _ []         = []
losPrimeros 0 _        = []
losPrimeros n (x:xs)    = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int-> [a]-> [a] -- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
                                    --recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs  = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

--  3. Registros --

--1--

data Persona = P String Int -- Nombre Edad 
    deriving (Show, Eq)

persona_1 = P "A" 20
persona_2 = P "B" 30

edad :: Persona-> Int -- Estoy definiendo mi funcion observadora, de la edad de una persona

edad (P n e) = e

mayoresA :: Int-> [Persona]-> [Persona] -- Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA 0 _        = []
mayoresA n []       = []
mayoresA n (p:ps)   = if edad p > n
                            then p : mayoresA n ps
                                        else mayoresA n ps

promedioEdad :: [Persona]-> Int -- Dada una lista de personas devuelve el promedio de edad entre esas personas. 
                                -- Precondición: la lista al menos posee una persona
promedioEdad ps = div (sumatoria (edades ps)) (longitud ps)

edades :: [Persona] -> [Int]                --Dada una lista de personas, devuelve una lista con las edades de esas personas
                                            -- Precondición: la lista al menos posee una persona
edades []       = error "La lista no puede ser vacia"
edades [p]      = [edad p]
edades (p:ps)   = edad p : edades ps

sumarTodos :: [Persona] -> Int -- Dada una lista de personas devuelve la suma de edad entre esas personas.
                               -- Precondición: la lista al menos posee una persona
sumarTodos []     = error "La lista no puede ser vacia"
sumarTodos [p]    = edad p
sumarTodos (p:ps) = edad p + sumarTodos ps


elMasViejo :: [Persona]-> Persona --Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo []  = error "La lista no puede ser vacia"
elMasViejo [p] = p
elMasViejo ps  = laQueEsMayor (elPrimero ps) (elPrimero (sinElPrimero ps))


--2--

data TipoDePokemon = Agua | Fuego | Planta
    deriving (Show, Eq)
data Pokemon = ConsPokemon TipoDePokemon Int-- 
    deriving Show
data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show

poke_1 = ConsPokemon Agua 50
poke_2 = ConsPokemon Fuego 48
poke_3 = ConsPokemon Agua 32
poke_4 = ConsPokemon Fuego 32

entrenador_1 = ConsEntrenador "A" [poke_3, poke_1, poke_2]
entrenador_2 = ConsEntrenador "B" [poke_2, poke_4]
--a

cantPokemon :: Entrenador-> Int --Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon (ConsEntrenador _ xs) = longitud xs

--b

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ xs) = cantPokesDe t xs

cantPokesDe :: TipoDePokemon -> [Pokemon] -> Int
cantPokesDe _ []     = 0
cantPokesDe t (x:xs) = unoSiEsMismoTipo t (tipoDe x) + cantPokesDe t xs

unoSiEsMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEsMismoTipo Agua Agua     = 1
unoSiEsMismoTipo Fuego Fuego   = 1
unoSiEsMismoTipo Planta Planta = 1
unoSiEsMismoTipo    _     _    = 0


--c
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon-> Entrenador-> Entrenador-> Int -- Dados dos entrenadores, indica la cantidad de Pokemon 
                                                                                        -- de cierto tipo pertenecientes al primer entrenador, 
                                                                                        -- que le ganarían a todos los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ ps1) (ConsEntrenador _ ps2) = cantidadDeTipo_De_SuperanA_ t ps1 ps2

cantidadDeTipo_De_SuperanA_ :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cantidadDeTipo_De_SuperanA_ t [] _          = 0
cantidadDeTipo_De_SuperanA_ t xs []         = cantDeTipo t xs
cantidadDeTipo_De_SuperanA_ t (x:xs) ys = if esDeTipo t (tipoDe x)
                                            then unoSileGanaATodos x ys + cantidadDeTipo_De_SuperanA_ t xs ys
                                            else cantidadDeTipo_De_SuperanA_ t xs ys

cantDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantDeTipo t []     = 0
cantDeTipo t (x:xs) = unoSiEsMismoTipo t (tipoDe x) + cantDeTipo t xs

esDeTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeTipo Agua Agua     = True
esDeTipo Fuego Fuego   = True
esDeTipo Planta Planta = True
esDeTipo    _     _    = False

tipoDe:: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t _) = t

unoSileGanaATodos :: Pokemon -> [Pokemon] -> Int
unoSileGanaATodos _ []     = 1
unoSileGanaATodos p (x:xs) = unoSi_SuperaA_ p x * unoSileGanaATodos p xs -- si a por lo menos 1 no le gana, ya me da 0 

unoSi_SuperaA_ :: Pokemon -> Pokemon -> Int
unoSi_SuperaA_ (ConsPokemon t1 _) (ConsPokemon t2 _) = unoSiEsTipoSuperior t1 t2

unoSiEsTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Int
-- Dados dos tipos de Pokemon, devuelve 1 si el primero es superior al segundo
unoSiEsTipoSuperior Agua   Fuego  = 1
unoSiEsTipoSuperior Fuego  Planta = 1
unoSiEsTipoSuperior Planta Agua   = 1
unoSiEsTipoSuperior   _      _    = 0


--d 
esMaestroPokemon :: Entrenador -> Bool --Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon (ConsEntrenador _ ps) = hayDeLosTresTipos ps

hayDeLosTresTipos :: [Pokemon] -> Bool --Dada una lista de pokemon, indica si en la misma hay al menos un pokemon de cada tipo.
hayDeLosTresTipos [] = False
hayDeLosTresTipos ps =  hayPokemonDe Agua ps && hayPokemonDe Fuego ps && hayPokemonDe Planta ps

hayPokemonDe :: TipoDePokemon -> [Pokemon] -> Bool
hayPokemonDe _ []     = False
hayPokemonDe t (p:ps) = elPokeEsDeTipo t p || hayPokemonDe t ps

elPokeEsDeTipo :: TipoDePokemon -> Pokemon -> Bool
elPokeEsDeTipo t1 (ConsPokemon t2 _) = esMismoTipo t1 t2

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua Agua     = True
esMismoTipo Fuego Fuego   = True
esMismoTipo Planta Planta = True
esMismoTipo    _     _    = False

---------------3.ROL ---------------------

data Seniority = Junior | SemiSenior | Senior
    deriving Show

data Proyecto = ConsProyecto String
    deriving Show

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show

data Empresa = ConsEmpresa [Rol]
    deriving Show


proy1 = ConsProyecto "Proy A"
proy2 = ConsProyecto "Proy B"

rol1 = Developer Junior proy1
rol2 = Management Senior proy2

empresa = ConsEmpresa [rol1, rol2]

proyectos :: Empresa -> [Proyecto] -- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos (ConsEmpresa rs) = proyectosSinRepetidos rs


proyectosSinRepetidos :: [Rol] -> [Proyecto] -- Dada una lista de roles, devuelve una lista sin proyectos repetidos. 
proyectosSinRepetidos [] = []
proyectosSinRepetidos (r:rs) = agregarALaListaSiNoEsta (proyectoDe r) (proyectosSinRepetidos rs)

agregarALaListaSiNoEsta :: Proyecto ->  [Proyecto] -> [Proyecto]
agregarALaListaSiNoEsta p ps = if proyectoPerteneceALaLista p ps then ps else p : ps

proyectoDe :: Rol -> Proyecto  -- Dado un rol, devuelve el proyecto
proyectoDe (Developer _ p) = p
proyectoDe (Management _ p) = p


proyectoPerteneceALaLista :: Proyecto -> [Proyecto] -> Bool
proyectoPerteneceALaLista _ [] = False
proyectoPerteneceALaLista x (n:ns) =  (nombreProy x)== (nombreProy n) || (proyectoPerteneceALaLista x ns)

nombreProy :: Proyecto -> String
nombreProy (ConsProyecto n) = n

losDevSenior :: Empresa-> [Proyecto]-> Int -- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
                                                -- además a los proyectos dados por parámetro.
losDevSenior (ConsEmpresa rs) ps = cantidadDevSeniorYDeProyectos rs ps 

cantidadDevSeniorYDeProyectos :: [Rol] -> [Proyecto] -> Int
cantidadDevSeniorYDeProyectos [] _      = 0
cantidadDevSeniorYDeProyectos (r:rs) ps = unoSiEsDevSeniorYSuProyectoPerteneceA (seniority r) (proyectoDe r) ps + cantidadDevSeniorYDeProyectos rs ps

unoSiEsDevSeniorYSuProyectoPerteneceA :: Seniority -> Proyecto -> [Proyecto] -> Int
unoSiEsDevSeniorYSuProyectoPerteneceA Senior p ps = unoSiSuProyectoPerteneceA p ps 
unoSiEsDevSeniorYSuProyectoPerteneceA _      _ _  = 0

unoSiSuProyectoPerteneceA :: Proyecto -> [Proyecto] -> Int 
unoSiSuProyectoPerteneceA p ps = if proyectoPerteneceALaLista p ps then 1 else 0

seniority :: Rol -> Seniority 
seniority (Developer s _) = s
seniority (Management s _) = s

cantQueTrabajanEn :: [Proyecto]-> Empresa-> Int --Indica la cantidad de empleados que trabajan en alguno de los proyectos dados
cantQueTrabajanEn ps (ConsEmpresa rs) = cantQueTrabajan ps rs

cantQueTrabajan :: [Proyecto]-> [Rol] -> Int 
cantQueTrabajan _ [] = 0
cantQueTrabajan ps (r:rs) = unoSiTrabajaEnEnAlgunProyecto ps r + cantQueTrabajan ps rs

unoSiTrabajaEnEnAlgunProyecto :: [Proyecto] -> Rol -> Int
unoSiTrabajaEnEnAlgunProyecto ps (Developer _ p)  = unoSiSuProyectoPerteneceA p ps
unoSiTrabajaEnEnAlgunProyecto ps (Management _ p) = unoSiSuProyectoPerteneceA p ps

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = cantPorProyecto rs

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

---AUXILIARES PRACTICA 1  --

sinElPrimero :: [a]-> [a]
sinElPrimero (_:xs) = xs

elPrimero :: [a]-> a
elPrimero (x:_) = x

esMayorQueLaOtra :: Persona-> Persona-> Bool

esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona-> Persona-> Persona

laQueEsMayor p1@(P _ e1) p2@(P _ e2) = if e1 >= e2
           then p1
           else p2

--------------- FIN AUXILIARES PRACTICA 1  -----------------
