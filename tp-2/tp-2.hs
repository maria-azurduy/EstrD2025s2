--  1. RECURSION SOBRE LISTAS
-- Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique lo contrario:
-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int

-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud :: [a] -> Int

-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]

-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool

-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool

-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]

-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool

-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int

-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]

-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]

-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al ?nal de la lista.
agregarAlFinal :: [a] -> a -> [a]

-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. De?nida en Haskell como (++).
agregar :: [a] -> [a] -> [a]

-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa :: [a] -> [a]

-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]

-- Dada una lista devuelve el mínimo
elMinimo :: Ord a => [a] -> a


-- 2. RECURSION SOBRE NUMEROS
-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:
-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int

--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]

-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]

-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
-- Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]

-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]

-- 3. REGISTROS
{-   
    3.1
    Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:
    Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
-}
mayoresA :: Int -> [Persona] -> [Persona]

-- Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int

-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona

{-
    3.2
    Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:
-}

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

-- Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
-- Definir en base a esa representación las siguientes funciones:

-- Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int

-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int

-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int

-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool

{-
    3.3
    El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
    de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
    una lista de personas con diferente rol. La de?nición es la siguiente:
-}

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

-- Definir las siguientes funciones sobre el tipo Empresa:

-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]

-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int

-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int

-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
