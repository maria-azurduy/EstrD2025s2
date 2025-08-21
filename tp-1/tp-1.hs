--NUMEROS ENTEROS

-- Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor n = n + 1


--Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar n m = n + m


{-Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.
-}
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m )

--Dado un par de números devuelve el mayor de estos
maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = max n m


--TIPOS ENUMERATIVOS
{-
Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
las siguientes funciones:
a) opuesto :: Dir -> Dir
Dada una dirección devuelve su opuesta.
b) iguales :: Dir -> Dir -> Bool
Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
c) siguiente :: Dir -> Dir
Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¾Posee una precondición esta función? Es una función
total o parcial? Por qué?
-}

data Dir = Norte | Sur | Este | Oeste 
    deriving Show

opuesto :: Dir -> Dir

opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True 
iguales Este Este = True 
iguales Oeste Oeste = True 
iguales _ _ = False

siguiente :: Dir -> Dir
siguiente Norte = Este 
siguiente Este = Sur
siguiente Sur = Oeste 
siguiente Oeste = Norte 

{-
Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:
-}

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show 

{-
Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar definir subtareas útiles
que puedan servir después.
-}
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

-- subtareas 

primerDia :: DiaDeSemana

primerDia = Lunes

ultimoDia :: DiaDeSemana

ultimoDia = Domingo


--Dado un día de la semana indica si comienza con la letra M.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Miercoles = True
empiezaConM Martes    = True


{-
Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
Ejemplo: vieneDespues Jueves Lunes = True
-}

-- auxiliar

indiceDia :: DiaDeSemana -> Int

indiceDia Lunes = 1
indiceDia Martes = 2
indiceDia Miercoles = 3
indiceDia Jueves = 4
indiceDia Viernes = 5
indiceDia Sabado = 6
indiceDia Domingo = 7

-- fin auxiliar

vieneDespues :: DiaDeSemana-> DiaDeSemana-> Bool
vieneDespues dia dia2 = indiceDia dia  > indiceDia dia2



--Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True 


{-
Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):
-}

{-
Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está definida como not.
-}
negar :: Bool -> Bool
negar True = False
negar False = True 
{-
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.
Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell.
-}
implica :: Bool -> Bool -> Bool
implica True b = False
implica False _ = True

{-
Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.
-}
yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien False _ = True 

{-Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||.
-}
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien False b = b 

--REGISTROS

-- Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:
data Persona = P String Int
    deriving Show
p1 = P "Pato" 25
p2 = P "Beto" 33

-- Devuelve el nombre de una persona
nombre :: Persona -> String          -- Estoy definiendo mi funcion observadora, del nombre de una persona
nombre (P n e) = n

-- Devuelve la edad de una persona
edad :: Persona -> Int
edad (P n e) = e

-- Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer (P n e) = P n (e +1)

-- Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nom (P _ e) = P nom e 

-- Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

-- Dadas dos personas devuelve a la persona que sea mayor
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1@(P _ e1) p2@(P _ e2) = if e1 > e2 then p1 else p2

{-
Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones:
-}
data TipoDePokemon = Agua | Fuego | Planta
    deriving (Show, Eq)
data Pokemon = PK TipoDePokemon Int -- TipoDePokemon Energia 
    deriving (Show)
data Entrenador = E String Pokemon Pokemon -- Nombre Pokemon Pokemon
    deriving (Show)

--ejemplos

maria = E "Maria" picachu roan
juan = E "Juan" aries santi 


picachu = PK Fuego 24 
roan = PK Planta 12
aries = PK Agua 45
santi = PK Fuego 42

{-
Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
-}
superaA :: Pokemon -> Pokemon -> Bool
superaA (PK tp1 int) (PK tp2 int1) = if tp1 == Agua && tp2 == Fuego then True 
                                     else if tp1 == Fuego && tp2 == Planta then True 
                                     else if tp1 == Planta && tp2 == Agua then True
                                    else False 


cantidadDePokemonDe :: TipoDePokemon-> Entrenador-> Int -- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
-- Solucion inicial

cantidadDePokemonDe tipo (E _ pk1 pk2) = (if tipoDe pk1 == tipo
                                            then 1
                                            else 0) 
                                        +

                                        (if tipoDe pk2 == tipo
                                            then 1
                                            else 0)

tipoDe :: Pokemon -> TipoDePokemon -- Devuelve el tipo del pokemon 

tipoDe (PK tp _) = tp 

--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) =  pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe :: Entrenador -> [Pokemon] 
pokemonesDe (E _ pk1 pk2) = pk1 : [pk2]

--FUNCIONES POLIMORFICAS

-- 1. Defina las siguientes funciones polimórficas:
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a
loMismo x = x

--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete x = 7

--Dadas una tupla, invierte sus componentes.
swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)



--PATTERN MATCHING SOBRE LISTAS

{-
1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):
-}

--Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
--Definida en Haskell como null
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

--Dada una lista devuelve su primer elemento.
--Definida en Haskell como head.
-- Precondicion: la lista NO debe ser vacía
elPrimero :: [a] -> a
elPrimero (x:_) = x


--Dada una lista devuelve esa lista menos el primer elemento.
--Definida en Haskell como tail.
sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs

--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
-- Precondicion: la lista NO debe ser vacía
splitHead :: [a] -> (a, [a])
splitHead xs = (elPrimero xs, sinElPrimero xs)
