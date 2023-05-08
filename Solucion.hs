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
pertenece a (x:xs) | length (x:xs) == 1 && a == x = True
                   | length (x:xs) == 1 && a /= x = False
                   | otherwise = a == x || pertenece a xs



usuarioValido :: Usuario -> Bool
usuarioValido a | idDeUsuario a > 0 && length (nombreDeUsuario a) > 0 = True
                | otherwise = False

--noHayIdsRepetidos :: [Usuario] -> Bool
--noHayIdsRepetidos 


hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos (x:xs) | length (x:xs) == 1 = False
                    | pertenece x xs = True
                    | otherwise = hayRepetidos xs



mismosElementos :: Int -> Int -> Bool
mismosElementos a b | a == b = True
                    | otherwise = False





--SANDBOX

usuario1 = (1, "Juan")