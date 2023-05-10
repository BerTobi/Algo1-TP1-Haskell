-- Completar con los datos del grupo
--
-- Nombre de Grupo: SegFault
-- Integrante 1: Tobias Bersia, tobiasbersia@gmail.com, 598/23
-- Integrante 2: Theo Janices, theo.janices@gmail.com, 895/23
-- Integrante 3: Miguel David Avendaño Padilla, mdavendano1612@gmail.com, 775/23
-- Integrante 4: Gonzalo Agustín Heredia, gonzalo.heredia.gh8@gmail.com, 948/23

module Solucion where

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined





--PREDICADOS Y FUNCIONES AUXILIARES

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece a xs | length xs == 0 = False
               | a == (head xs) = True
               | otherwise = pertenece a (tail xs)


mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys | xs == [] && ys == [] = True
                      | xs == [] && ys /= [] = False
                      | xs /= [] && ys == [] = False
                      | pertenece (head xs) ys == False = False
                      | pertenece (head xs) ys == True = mismosElementos (quitarTodos (head xs) xs) (quitarTodos (head xs) ys)

--Sirve para mismosElementos
quitarTodos :: (Eq t) => t -> [t] -> [t] 
quitarTodos x xs | xs == [] = []
                 | x == head xs = quitarTodos x (tail xs)
                 | otherwise = [head xs] ++ quitarTodos x (tail xs)



usuarioValido :: Usuario -> Bool
usuarioValido a | idDeUsuario a > 0 && length (nombreDeUsuario a) > 0 = True
                | otherwise = False



noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (x:xs) | length listaDeIds == 1 = True
                         | pertenece (head listaDeIds) (tail listaDeIds) = False 
                         | otherwise = noHayIdsRepetidos xs
                    where
                        listaDeIds = construccionListaIds (x:xs)


--Me hago una sola lista con las ids para noHayIdsRepetidos
construccionListaIds :: [Usuario] -> [Integer]
construccionListaIds [] = []
construccionListaIds (x:xs) | length (x:xs) == 1 = [fst x]
                            | otherwise = [fst x] ++ construccionListaIds xs




usuariosValidos :: [Usuario] -> Bool
usuariosValidos (x:xs) | length (x:xs) == 1 && usuarioValido (head (x:xs)) = True
                       | otherwise = usuarioValido x && noHayIdsRepetidos (x:xs) && usuariosValidos xs




--SANDBOX

--Para que sea mas facil probar
usuario1 = (1, "Juan")
usuario2 = (5, "Roberto")
usuario3 = (4, "")
usuario4 = (6, "Roberto")


--Actualmente no funciona en ninguna función, pero como hay otros de no repetidos quiza ustedes le encuentren alguna utilidad.
noHayRepetidos :: (Eq t) => [t] -> Bool
noHayRepetidos (x:xs) | length (x:xs) == 1 = True
                      | pertenece x xs = False
                      | otherwise = noHayRepetidos xs