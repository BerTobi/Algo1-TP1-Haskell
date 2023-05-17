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

-- describir qué hace la función: Le entregas una red social y te devuelve una lista con todos los nombres de los usuarios
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = eliminarRepetidos (construccionListaNombres (usuarios red))

-- aux de nombresDeUsuarios: Construye una lista de nombres de usuarios, contiene repeticiones.
construccionListaNombres :: [Usuario] -> [String]
construccionListaNombres [] = []
construccionListaNombres (x:xs) | length (x:xs) == 1 = [snd x]
                                | otherwise = [snd x] ++ construccionListaNombres xs


-- describir qué hace la función: Dada una red social y un usuario, devuelve una lista de usuarios los cuales son amigos del usuario enviado como imput.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red usA = eliminarRepetidos (construccionListaUsuarios ((filtroDeRelaciones usA) (relaciones red)) usA)

-- aux de Amigos de. Filtra todas las relaciones en la que aparezca el usuario provisto.
filtroDeRelaciones :: Usuario -> [Relacion] -> [Relacion]
filtroDeRelaciones usA [] = []
filtroDeRelaciones usA listaDeRel | length listaDeRel == 1 && ((fst (head listaDeRel)) == usA || (snd (head listaDeRel)) == usA) = [(head listaDeRel)]
                                  | (fst (head listaDeRel) == usA || snd (head listaDeRel) == usA) = [(head listaDeRel)] ++ filtroDeRelaciones usA (tail listaDeRel)
                                  | otherwise = filtroDeRelaciones usA (tail listaDeRel)

--aux de Amigos de. Con una lista de todas la relaciones en la que aparezca un usuario, entrega una lista de todas las personas con la cual está relacionada.
construccionListaUsuarios :: [Relacion] -> Usuario -> [Usuario]
construccionListaUsuarios [] _ = []
construccionListaUsuarios (x:xs) usA | snd x == usA && length (x:xs) == 1 = [fst x]
                                     | snd x == usA = [fst x] ++ construccionListaUsuarios xs usA
                                     | fst x == usA && length (x:xs) == 1 = [snd x]
                                     | otherwise = [snd x] ++ construccionListaUsuarios xs usA



-- describir qué hace la función: Recibe una red social y un usuario y devuelve la cantidad de amigos que tiene el usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usA = length (amigosDe red usA) 

-- describir qué hace la función: Recibe una red social y devuelve el usuario con más amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosDeLista (usuarios red) red

usuarioConMasAmigosDeLista :: [Usuario] -> RedSocial -> Usuario
usuarioConMasAmigosDeLista (usuario:usuarios) red | length usuarios == 0 = usuario
                                                  | cantidadDeAmigos red usuario >= cantidadDeAmigos red (usuarioConMasAmigosDeLista usuarios red) = usuario
                                                  | otherwise = usuarioConMasAmigosDeLista usuarios red

-- describir qué hace la función: determina si existe un usuario que tenga más de 10 amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | cantidadDeAmigos red (usuarioConMasAmigos red) > 10 = True
                      | otherwise = False

-- describir qué hace la función: devuelve una lista, sin elementos repetidos, de las publicaciones realizadas por el usuario en la red social.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u | not(hayRepetidos (publicaciones red)) && pertenece u (usuarios red) = publicacionesDelUsuario u (publicaciones red)
                      | otherwise = []

publicacionesDelUsuario :: Usuario -> [Publicacion] -> [Publicacion]
publicacionesDelUsuario u [] = []
publicacionesDelUsuario u pubs | usuarioDePublicacion (head pubs) == u = head pubs : publicacionesDelUsuario u (tail pubs)
                               | otherwise = publicacionesDelUsuario u (tail pubs)

-- describir qué hace la función: Dados una red social y un usuario, devuelve las publicaciones que le gustan al usuario.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us | pertenece us (usuarios red) = publicacionyusuario us (publicaciones red)


publicacionyusuario :: Usuario -> [Publicacion] -> [Publicacion]
publicacionyusuario us pubs| length pubs == 0 = []
                           | pertenece us (likesDePublicacion (head pubs)) = head (pubs) : publicacionyusuario us (tail pubs)
                           | otherwise = publicacionyusuario us (tail pubs)

-- describir qué hace la función: Dada una red social y 2 usuarios de la red, se determina si les gustan las mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red us1 us2| pertenece us1 (usuarios red) && pertenece us2 (usuarios red) && (publicacionesQueLeGustanA red us1 == publicacionesQueLeGustanA red us2) = True
                                           | otherwise = False

-- describir qué hace la función: Determina si existe un usuario que le gustan todas las publicaciones de un usuario dado.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u | existeSeguidorFiel (usuarios red) u red = True
                          | otherwise = False

existeSeguidorFiel :: [Usuario] -> Usuario -> RedSocial -> Bool
existeSeguidorFiel [] u red = False
existeSeguidorFiel usl u red | (publicacionesDe red u) == [] = False
                             | (u /= head usl) && (contenido (publicacionesDe red u) (publicacionesQueLeGustanA red (head usl))) = True
                             | otherwise = existeSeguidorFiel (tail usl) u red

-- describir qué hace la función: Recibe una red y dos usuario, y devuelve true si existe una cadena de amigos entre ambos usuarios.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red us1 us2 | pertenece us2 (todosRelacionados red us1 (amigosDe red us1) (relaciones red)) = True
                                    | otherwise = False

todosRelacionados :: RedSocial -> Usuario -> [Usuario] -> [Relacion] -> [Usuario]
todosRelacionados red us1 relacionados relaciones | length relaciones == 0 = relacionados
                                                  | length relaciones >= 1 && pertenece (fst (head relaciones)) relacionados == True && pertenece (snd (head relaciones)) relacionados == False = todosRelacionados red us1 (relacionados ++ [(snd (head relaciones))]) relaciones
                                                  | length relaciones >= 1 && pertenece (fst (head relaciones)) relacionados == False && pertenece (snd (head relaciones)) relacionados == True = todosRelacionados red us1 (relacionados ++ [(fst (head relaciones))]) relaciones
                                                  | otherwise = todosRelacionados red us1 relacionados (tail relaciones)

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
hayRepetidos [] = False
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
redSocialValida :: RedSocial -> Bool
redSocialValida red | usuariosValidos us == True && relacionesValidas us rels == True && publicacionesValidas us pubs == True = True
                    | otherwise = False
                where us = usuarios red
                      rels = relaciones red
                      pubs = publicaciones red


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
                          | length (x:xs) == 2 && relacionadosDirecto x (head xs) red = True
                          | relacionadosDirecto x (head xs) red && cadenaDeAmigos xs red = True
                          | otherwise = False

-- Funciones Auxiliares

quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs | xs == [] = []
            | x == head xs = tail xs
            | otherwise = [head xs] ++ quitar x (tail xs)

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | length (x:xs) == 1 = (x:xs)
                         | pertenece x xs == False = [x] ++ eliminarRepetidos xs
                         | otherwise = eliminarRepetidos ([x] ++ quitarTodos x xs)

contenido :: (Eq t) => [t] -> [t] -> Bool
contenido [] ys = True
contenido xs [] = False
contenido xs ys | pertenece (head xs) ys = contenido (tail xs) ys
                | not(pertenece (head xs) ys) = False