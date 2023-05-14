-- Completar con los datos del grupo
--
-- Nombre de Grupo: SegFault
-- Integrante 1: Tobias Bersia, tobiasbersia@gmail.com, 598/23
-- Integrante 2: Theo Janices, theo.janices@gmail.com, 895/23
-- Integrante 3: Miguel David Avendaño Padilla, mdavendano1612@gmail.com, 775/23
-- Integrante 4: Gonzalo Agustín Heredia, gonzalo.heredia.gh8@gmail.com, 948/23

module Solucion where
import Data.Time.Format.ISO8601 (yearFormat)

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


--PREDICADOS BASICOS INDEPENDIENTES

--I)
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece a xs | length xs == 0 = False
               | a == (head xs) = True
               | otherwise = pertenece a (tail xs)

--II)
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys | xs == [] && ys == [] = True
                      | xs == [] && ys /= [] = False
                      | xs /= [] && ys == [] = False
                      | pertenece (head xs) ys == False = False
                      | pertenece (head xs) ys == True = mismosElementos (quitarTodos (head xs) xs) (quitarTodos (head xs) ys)
                      
--II) aux
quitarTodos :: (Eq t) => t -> [t] -> [t] 
quitarTodos x xs | xs == [] = []
                 | x == head xs = quitarTodos x (tail xs)
                 | otherwise = [head xs] ++ quitarTodos x (tail xs)

--III)
empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon x xs | xs == [] = False
                | x == head xs = True
                | otherwise = False

--IV)
terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon x xs | xs == [] = False
                | length xs == 1 && x == head xs = True
                | length xs == 1 && x /= head xs = False
                | otherwise = terminaCon x (tail xs)

--V)
sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos (x:xs) | length (x:xs) == 1 = True
                    | pertenece x xs = False
                    | otherwise = sinRepetidos xs
                 
--1)
usuarioValido :: Usuario -> Bool
usuarioValido a | idDeUsuario a > 0 && length (nombreDeUsuario a) > 0 = True
                | otherwise = False
                
--2)
noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (x:xs) | length listaDeIds == 1 = True
                         | pertenece (head listaDeIds) (tail listaDeIds) = False 
                         | otherwise = noHayIdsRepetidos xs
                    where
                        listaDeIds = construccionListaIds (x:xs)
--2) Aux
--Me hago una sola lista con las ids para noHayIdsRepetidos
construccionListaIds :: [Usuario] -> [Integer]
construccionListaIds [] = []
construccionListaIds (x:xs) | length (x:xs) == 1 = [fst x]
                            | otherwise = [fst x] ++ construccionListaIds xs

--3)
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos us rels | length rels == 0 = True
                                  | length rels > 0 && validezDeRelacionCheck us (head rels) == True = usuariosDeRelacionValidos us (tail rels)
                                  | otherwise = False

--3) Aux
validezDeRelacionCheck :: [Usuario] -> Relacion -> Bool
validezDeRelacionCheck us rel | fst rel == snd rel = False
                              | pertenece (fst rel) us && pertenece (snd rel) us = True
                              | otherwise = False

--4)
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas rels | length rels == 0 = True
                           | relacionSimetrica (head rels) (tail rels) == True = False
                           | otherwise = relacionesAsimetricas (tail rels)
                    
--4) Aux
relacionSimetrica :: Relacion -> [Relacion] -> Bool
relacionSimetrica rel rels | pertenece ((snd rel), (fst rel)) rels = True
                           | otherwise = False

--5)
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas rels | length rels == 1 || length rels == 0 = True
                              | length rels > 0 && chequeoDeRelacion (head rels) (tail rels) == True = noHayRelacionesRepetidas (tail rels)
                              | otherwise = False
--5) Aux
chequeoDeRelacion :: Relacion -> [Relacion] -> Bool
chequeoDeRelacion r rels | length rels == 0 = True
                         | (fst r == fst (head rels) && snd r == snd (head rels))= False 
                         | otherwise = chequeoDeRelacion r (tail rels)

--6)
--Sirve para e)
usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos us [] = True
usuariosLikeValidos us usl | pertenece (head usl) us = usuariosLikeValidos us (tail usl)
                           | not(pertenece (head usl) us) = False

--7)
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto usA usB red | pertenece (usA, usB) (relaciones red) || pertenece (usB, usA) (relaciones red) = True
                                | otherwise = False

--8)
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed us [] = True
usuariosDePublicacionSonUsuariosDeRed us pubs | pertenece (usuarioDePublicacion (head pubs)) us = usuariosDePublicacionSonUsuariosDeRed us (tail pubs)
                                              | not(pertenece (usuarioDePublicacion (head pubs)) us) = False

--9)
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas pubs | hayRepetidos pubs = False
                                 | otherwise = True
--9) Aux                                 
--Sirve para noHayPublicacionesRepetidas
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos (x:xs) | length (x:xs) == 1 = False
                    | pertenece x xs = True
                    | otherwise = hayRepetidos xs

--10)
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red [] = False
sonDeLaRed red (x:xs) | length (x:xs) == 1 && pertenece (x) (usuarios red) = True
                      | length (x:xs) > 1 && pertenece (x) (usuarios red) == True = sonDeLaRed red xs
                      | otherwise = False


-- PREDICADOS DEPENDIENTES A BASICOS

--a)
redSocialValida :: [Usuario] -> [Relacion] -> [Publicacion] -> Bool
redSocialValida us rels pubs| usuariosValidos us == True && relacionesValidas us rels == True && publicacionesValidas us pubs == True = True
                            | otherwise = False

--b)
usuariosValidos :: [Usuario] -> Bool
usuariosValidos (x:xs) | length (x:xs) == 1 && usuarioValido (head (x:xs)) = True
                       | otherwise = usuarioValido x && noHayIdsRepetidos (x:xs) && usuariosValidos xs

--c) 
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels | usuariosDeRelacionValidos us rels == True && relacionesAsimetricas rels == True && noHayRelacionesRepetidas rels == True = True
                          | otherwise = False

--d)
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs | usuariosDePublicacionSonUsuariosDeRed us pubs && usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs && noHayPublicacionesRepetidas pubs = True
                             | otherwise = False            

--e)
usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed us [] = True
usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs | usuariosLikeValidos us (likesDePublicacion (head pubs)) = usuariosDeLikeDePublicacionSonUsuariosDeRed us (tail pubs)
                                                    | not(usuariosLikeValidos us (likesDePublicacion (head pubs))) = False

--f)
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] red = False
cadenaDeAmigos (x:xs) red | length (x:xs) == 1 = False
                          | relacionadosDirecto x (head xs) red || cadenaDeAmigos xs red = True
                          | otherwise = cadenaDeAmigos xs red

-- Funciones Auxiliares

quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs | xs == [] = []
            | x == head xs = tail xs
            | otherwise = [head xs] ++ quitar x (tail xs)

--SANDBOX
--Para que sea mas facil probar
{-
--Usuarios
usuario1 = (1, "Juan")
usuario2 = (5, "Roberto")
usuario3 = (4, "")
usuario4 = (6, "Roberto")

--Publicaciones
publicacion1 = (usuario1, "Primer post", [usuario2, usuario3, usuario4])
publicacion2 = (usuario2, "Hola mundo", [usuario1, usuario3, usuario4])

relacion1 = (usuario1, usuario2)
relacion2 = (usuario1, usuario1)

redSocialA = ([usuario1, usuario2, usuario3, usuario4], [relacion1, relacion2], [publicacion1])
-}