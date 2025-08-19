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
divisionYResto n m = (div n m, mod (div n m))

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
primeroYUltimoDia (d1, d2) = (primerDia, ultimoDia)

primerDia :: DiaDeSemana -> DiaDeSemana
primerDia Lunes = True
primerDia _ = False 

ultimoDia :: DiaDeSemana -> DiaDeSemana
ultimoDia Domingo = True
ultimoDia _ = False 


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
estaEnElMedio primerDia = False
estaEnElMedio ultimoDia = False
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


{-
Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.
-}
yTambien :: Bool -> Bool -> Bool


{-Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||.
-}
oBien :: Bool -> Bool -> Bool


--REGISTROS

-- Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:
data Persona = P String Int
    deriving Show
p1 = P "A" 25
p2 = P "B" 33
-- Devuelve el nombre de una persona
nombre :: Persona -> String

-- Devuelve la edad de una persona
edad :: Persona -> Int

-- Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona

-- Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona

-- Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool

-- Dadas dos personas devuelve a la persona que sea mayor
laQueEsMayor :: Persona -> Persona -> Persona


{-
Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones:
-}
data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Entrenador = ConsEntrenador String Pokemon Pokemon
    deriving Show

poke_1 = ConsPokemon Agua 1
poke_2 = ConsPokemon Fuego 2
poke_3 = ConsPokemon Agua 3
poke_4 = ConsPokemon Fuego 4
poke_5 = ConsPokemon Fuego 5
poke_6 = ConsPokemon Planta 6
poke_7 = ConsPokemon Planta 7
entrenador_1 =ConsEntrenador "A" poke_3 poke_1
entrenador_2 =ConsEntrenador "B" poke_2 poke_4
entrenador_3 =ConsEntrenador "C" poke_2 poke_3
entrenador_4 =ConsEntrenador "D" poke_1 poke_6
entrenador_5 =ConsEntrenador "E" poke_1 poke_3


{-
Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
-}
superaA :: Pokemon -> Pokemon -> Bool



--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int



--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]



--FUNCIONES POLIMORFICAS

-- 1. Defina las siguientes funciones polimórficas:
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a


--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int


--Dadas una tupla, invierte sus componentes.
swap :: (a,b) -> (b, a)




--PATTERN MATCHING SOBRE LISTAS

{-
1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):
-}

--Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
--Definida en Haskell como null
estaVacia :: [a] -> Bool


--Dada una lista devuelve su primer elemento.
--Definida en Haskell como head.
--Nota: tener en cuenta que el constructor de listas es :
elPrimero :: [a] -> a
-- Precondicion: la lista NO debe ser vacía


--Dada una lista devuelve esa lista menos el primer elemento.
--Definida en Haskell como tail.
--Nota: tener en cuenta que el constructor de listas es :
sinElPrimero :: [a] -> [a]


--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
--Nota: tener en cuenta que el constructor de listas es :
splitHead :: [a] -> (a, [a])
-- Precondicion: la lista NO debe ser vacía

