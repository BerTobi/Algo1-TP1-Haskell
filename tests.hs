import Test.HUnit
import Solucion

predicados = runTestTT basicosIndependientes >> runTestTT dependientesDeBasicos

ejercicios = runTestTT auxDeEjercicios >> runTestTT testsEjercicios


basicosIndependientes = test [
    " siPertenece " ~: (pertenece siPerteneceElem siPerteneceLista) ~?= True,

    " noPertenece " ~: (pertenece noPerteneceElem noPerteneceLista) ~?= False,

    " Pertenece (lista vacia)" ~: (pertenece 1 listaVacia) ~?= False,

    " EmpiezaCon 1 " ~: (empiezaCon empiezaCon1elem empiezaCon1lista) ~?= True,

    " EmpiezaCon 2 " ~: (empiezaCon empiezaCon2elem empiezaCon2lista) ~?= False,

    " EmpiezaCon 3 " ~: (empiezaCon empiezaCon3elem empiezaCon3lista) ~?= False,

    " TerminaCon 1 " ~: (terminaCon terminaCon1elem terminaCon1lista) ~?= True,

    " TerminaCon 2 " ~: (terminaCon terminaCon2elem terminaCon2lista) ~?= False,

    " TerminaCon 3 " ~: (terminaCon terminaCon3elem terminaCon3lista) ~?= False,

    " usuarioValido 1" ~: (usuarioValido usuario1) ~?= True,

    " usuarioValido 2" ~: (usuarioValido usuarioInvalido1) ~?= False,

    " usuarioValido 3" ~: (usuarioValido usuarioInvalido2) ~?= False,

    " noHayIdsRepetidos 1" ~: (noHayIdsRepetidos usuarios4) ~?= True,

    " noHayIdsRepetidos 2" ~: (noHayIdsRepetidos usuarios6) ~?= False,

    " noHayIdsRepetidos 3" ~: (noHayIdsRepetidos listaVacia) ~?= True,

    " noHayIdsRepetidos 4" ~: (noHayIdsRepetidos [usuario1]) ~?= True,

    " UsuariosDeRelacionValidos 1 " ~: (usuariosDeRelacionValidos usuarios4 relaciones1) ~?= True,

    " UsuariosDeRelacionValidos 2 " ~: (usuariosDeRelacionValidos usuarios4 relacionesInvalidas1) ~?= False,

    " UsuariosDeRelacionValidos 3 " ~: (usuariosDeRelacionValidos usuarios1 relaciones1) ~?= False,

    " RelacionesAsimetricas 1 " ~: (relacionesAsimetricas relaciones1) ~?= True,

    " RelacionesAsimetricas 2 " ~: (relacionesAsimetricas relacionesInvalidas3) ~?= False,

    " RelacionesAsimetricas 3 " ~: (relacionesAsimetricas relacionesInvalidas4) ~?= False,
    
    " NoHayRelacionesRepetidas 1 " ~: (noHayRelacionesRepetidas relaciones1) ~?= True,

    " usuariosLikeValidos 1 " ~: (usuariosLikeValidos usuarios7 usuarios1) ~?= True,

    " usuariosLikeValidos 2 " ~: (usuariosLikeValidos usuarios7 usuarios2) ~?= False,

    " usuariosLikeValidos 3 " ~: (usuariosLikeValidos usuarios1 listaVacia) ~?= True,

    " NoHayRelacionesRepetidas 2 "  ~: (noHayRelacionesRepetidas relacionesInvalidas3) ~?= True,
   
    " NoHayRelacionesRepetidas 3 "  ~: (noHayRelacionesRepetidas relacionesInvalidas2) ~?= False,

    " siRelacionadosDirecto 1" ~: (relacionadosDirecto usuario1 usuario2 redSocialA) ~?= True,

    " noRelacionadosDirecto 2" ~: (relacionadosDirecto usuario1 usuario3 redSocialA) ~?= False,

    " noRelacionadosDirecto 3" ~: (relacionadosDirecto usuario5 usuario1 redSocialA) ~?= False,
    
    " usuariosDePublicacionSonUsuariosDeRed 1" ~: (usuariosDePublicacionSonUsuariosDeRed usuarios7 publicaciones1) ~?= True,
    
    " usuariosDePublicacionSonUsuariosDeRed 2" ~: (usuariosDePublicacionSonUsuariosDeRed usuarios5 publicacionesInvalidas4) ~?= False,
    
    " usuariosDePublicacionSonUsuariosDeRed 3" ~: (usuariosDePublicacionSonUsuariosDeRed usuarios3 listaVacia) ~?= True,

    " noHayPublicacionesRepetidas 1 " ~: (noHayPublicacionesRepetidas publicaciones1) ~?= True,
    
    " noHayPublicacionesRepetidas 2 " ~: (noHayPublicacionesRepetidas publicacionesInvalidas2) ~?= False,
    
    " noHayPublicacionesRepetidas 3 " ~: (noHayPublicacionesRepetidas listaVacia) ~?= True,
    
    " noHayPublicacionesRepetidas 4 " ~: (noHayPublicacionesRepetidas publicaciones2) ~?= True
 ]

dependientesDeBasicos = test [
    " usuariosValidos 1" ~: (usuariosValidos usuarios1) ~?= True,

    " usuariosValidos 2" ~: (usuariosValidos usuarios2) ~?= False,

    " usuariosValidos 3" ~: (usuariosValidos usuarios3) ~?= False,

    " usuariosValidos 4" ~: (usuariosValidos usuarios7) ~?= True,

    " RelacionesValidas 1 " ~: (relacionesValidas usuarios4 relacionesInvalidas3) ~?= False,

    " RelacionesValidas 2" ~: (relacionesValidas usuarios4 relaciones1) ~?= True,

    " RelacionesValidas 3" ~: (relacionesValidas usuarios1 relaciones1) ~?= False,

    " RedSocialValida 1 " ~: (redSocialValida redSocialA) ~?= True,

    " RedSocialValida 2 " ~: (redSocialValida redSocialH) ~?= False,

    " RedSocialValida 3 " ~: (redSocialValida redSocialI) ~?= False,

    " RedSocialValida 4 " ~: (redSocialValida redSocialJ) ~?= False,

    " publicacionesValidas 1" ~: (publicacionesValidas usuarios7 publicaciones1) ~?= True,
    
    " publicacionesValidas 2" ~: (publicacionesValidas usuarios5 publicacionesInvalidas4) ~?= False,
    
    " publicacionesValidas 3" ~: (publicacionesValidas usuarios7 publicacionesInvalidas3) ~?= False,
    
    " publicacionesValidas 4" ~: (publicacionesValidas usuarios1 publicacionesInvalidas2) ~?= False,
    
    " publicacionesValidas 5" ~: (publicacionesValidas usuarios1 listaVacia) ~?= True,

    " usuariosDeLikeDePublicacionSonUsuariosDeRed 1" ~: (usuariosDeLikeDePublicacionSonUsuariosDeRed usuarios7 publicaciones1) ~?= True,
    
    " usuariosDeLikeDePublicacionSonUsuariosDeRed 2" ~: (usuariosDeLikeDePublicacionSonUsuariosDeRed usuarios7 publicacionesInvalidas3) ~?= False,
    
    " usuariosDeLikeDePublicacionSonUsuariosDeRed 3" ~: (usuariosDeLikeDePublicacionSonUsuariosDeRed usuarios7 listaVacia) ~?= True,
    
    " usuariosDeLikeDePublicacionSonUsuariosDeRed 4" ~: (usuariosDeLikeDePublicacionSonUsuariosDeRed usuarios1 publicaciones2) ~?= False,

    " cadenaDeAmigos 1" ~: (cadenaDeAmigos usuarios4 redSocialC) ~?= True,

    " cadenaDeAmigos 2" ~: (cadenaDeAmigos usuarios5 redSocialB) ~?= False,

    " cadenaDeAmigos 3" ~: (cadenaDeAmigos listaVacia redSocialA) ~?= False
 ]

auxDeEjercicios = test [
    " filtroDeRelaciones 1" ~: (filtroDeRelaciones usuario1 relaciones1) ~?= [relacion1],

    " filtroDeRelaciones 2" ~: (filtroDeRelaciones usuario5 relaciones1) ~?= []

 ]



testsEjercicios = test [

    " nombresDeUsuarios" ~: (nombresDeUsuarios redSocialD) ~?= ["Juan","Natalia", "Pedro","Mariela"],

    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redSocialR) ~?= ["Juan", "Natalia", "Pedro", "Mariela", "Tomas"],   --Muchos nombres, en la red hay repetidos, pero en la lista de nombres no tiene que registrar a los repetidos

    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redSocialA) ~?= ["Juan", "Natalia", "Pedro", "Mariela"], --Sin repeticiones

    " amigosDe 1" ~: (amigosDe redSocialA usuario1) ~?= [usuario2],               -- usuario1 con un amigo
    
    " amigosDe 2" ~: (amigosDe redSocialE usuario6) ~?= [usuario1, usuario2, usuario3, usuario4, usuario5, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12],     -- Muchos amigos, los de Roberto Carlos

    " amigosDe 3" ~: (amigosDe redSocialB usuario3) ~?= [],                       -- No existe ninguna relacion del usuario 3 con otra persona en esta red social

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redSocialA usuario1) ~?= 1,        -- Cuentan la cantidad de usuarios resultado de los casos anteriores.

    " cantidadDeAmigos 2" ~: (cantidadDeAmigos redSocialE usuario6) ~?= 11,

    " cantidadDeAmigos 3" ~: (cantidadDeAmigos redSocialB usuario3) ~?= 0,

    " usuarioConMasAmigos 1" ~: (usuarioConMasAmigos redSocialA) ~?= usuario1,

    " usuarioConMasAmigos 2" ~: (usuarioConMasAmigos redSocialB) ~?= usuario1,

    " usuarioConMasAmigos 3" ~: (usuarioConMasAmigos redSocialD) ~?= usuario3,

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redSocialD) ~?= False,

    " estaRobertoCarlos 2" ~: (estaRobertoCarlos redSocialE) ~?= True,

    " publicacionesQueLeGustanA 1"  ~: (publicacionesQueLeGustanA redSocialO usuario1) ~?= publicaciones5,

    " publicacionesQueLeGustanA 2"  ~: (publicacionesQueLeGustanA redSocialO usuario2) ~?= publicaciones1,

    " publicacionesQueLeGustanA 3"  ~: (publicacionesQueLeGustanA redSocialP usuario1) ~?= [publicacion2,publicacion4],

    " lesGustanLasMismasPublicaciones 1" ~: (lesGustanLasMismasPublicaciones redSocialP usuario1 usuario2) ~?= False,

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redSocialQ usuario1 usuario2) ~?= True,
    
    " publicacionesDe 1" ~: (publicacionesDe redSocialE usuario1) ~?= [publicacion1],
    
    " publicacionesDe 2" ~: (publicacionesDe redSocialD usuario5) ~?= [],

    " publicacionesDe 3" ~: (publicacionesDe redSocialD usuarioInvalido1) ~?= [],

    " tieneUnSeguidorFiel " ~: (tieneUnSeguidorFiel redSocialK usuario1) ~?= True,

    " tieneUnSeguidorFiel " ~: (tieneUnSeguidorFiel redSocialL usuario1) ~?= False,

    " tieneUnSeguidorFiel " ~: (tieneUnSeguidorFiel redSocialK usuario3) ~?= False,

    " tieneUnSeguidorFiel " ~: (tieneUnSeguidorFiel redSocialM usuario1) ~?= False,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redSocialD usuario1 usuario5) ~?= True,

    " existeSecuenciaDeAmigos 2" ~: (existeSecuenciaDeAmigos redSocialA usuario1 usuario4) ~?= False,

    " existeSecuenciaDeAmigos 3" ~: (existeSecuenciaDeAmigos redSocialA usuario1 usuario2) ~?= True,

    " existeSecuenciaDeAmigos 3" ~: (existeSecuenciaDeAmigos redSocialE usuario12 usuario2) ~?= True

 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Casos

-- Pertenece
siPerteneceElem = 1
siPerteneceLista  = [2, 5, 1, 3, 2]
noPerteneceElem = 'b'
noPerteneceLista = ['z', 'c', 'd', 'a', 'e']
listaVacia = []

-- EmpiezaCon
empiezaCon1elem = 1
empiezaCon1lista = [1, 2, 3, 4]
empiezaCon2elem = 'a'
empiezaCon2lista = ['b', 'a', 'c', 'd']
empiezaCon3elem = 2
empiezaCon3lista = []

-- EmpiezaCon
terminaCon1elem = 1
terminaCon1lista = [4, 2, 3, 1]
terminaCon2elem = 'a'
terminaCon2lista = ['b', 'a', 'c', 'd']
terminaCon3elem = 2
terminaCon3lista = []

-- Usuarios Validos
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Roberto Carlos")
usuario7 = (7, "Jaimito")
usuario8 = (8, "Manuel")
usuario9 = (9, "Tomas")
usuario10 = (10, "Roman")
usuario11 = (11, "Napoleon")
usuario12 = (12, "Ramon")

usuario13 = (13, "Pedro")
usuario14 = (14, "Juan")
usuario15 = (15, "Natalia")

-- Usuarios Invalidos
usuarioInvalido1 = (0, "Juan")
usuarioInvalido2 = (2, "")
usuarioInvalido3 = (3, "Juan") -- (Utiliza la misma Id, caso de prueba para Ids Repetidos)

-- Listas de usuarios

usuarios1 = [usuario1, usuario2]
usuarios2 = [usuario3, usuario2, usuarioInvalido2]
usuarios3 = [usuario1, usuario5, usuario1]
usuarios4 = [usuario1, usuario2, usuario3, usuario4]
usuarios5 = [usuario1, usuario2, usuario3, usuario4, usuario5]
usuarios6 = [usuario3, usuario2, usuarioInvalido3]
usuarios7 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
usuarios8 = [usuario1, usuario6, usuario8]
usuarios9 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario9, usuario13, usuario14, usuario15]


-- Relaciones validas

relacion1 = (usuario1, usuario2)
relacion2 = (usuario4, usuario3)
relacion3 = (usuario2, usuario1)
relacion4 = (usuario3, usuario4)
relacion5 = (usuario1, usuario5)
relacion6 = (usuario2, usuario3)
relacion7 = (usuario3, usuario5)
relacion8 = (usuario1, usuario6)
relacion9 = (usuario2, usuario6)
relacion10 = (usuario3, usuario6)
relacion11 = (usuario4, usuario6)
relacion12 = (usuario5, usuario6)
relacion13 = (usuario7, usuario6)
relacion14 = (usuario8, usuario6)
relacion15 = (usuario9, usuario6)
relacion16 = (usuario10, usuario6)
relacion17 = (usuario11, usuario6)
relacion18 = (usuario12, usuario6)

-- Relaciones invalidas

relacionInvalida1 = (usuario1, usuario1)

--Lista de Relaciones Validas

relaciones1 = [relacion1, relacion2]
relaciones2 = []
relaciones3 = []
relaciones4 = []
relaciones5 = [relacion5, relacion3]
relaciones6 = [relacion1, relacion6, relacion2, relacion5]
relaciones7 = [relacion1, relacion4, relacion6, relacion7]
relaciones8 = [relacion1, relacion4, relacion6, relacion7, relacion8, relacion9, relacion10, relacion11, relacion12, relacion13, relacion14, relacion15, relacion16, relacion17, relacion18]

--Lista de Relaciones Invalidas

relacionesInvalidas1 = [relacion1, relacionInvalida1, relacion2]
relacionesInvalidas2 = [relacion1,relacion2,relacion3,relacion1]
relacionesInvalidas3 = [relacion1, relacion2, relacion3, relacion4]
relacionesInvalidas4 = [relacion1, relacion3, relacion2]

-- Publicaciones Validas

publicacion1 = (usuario1, "Hola", [(usuario2)])
publicacion2 = (usuario2, "Hola", (usuarios1))
publicacion3 = (usuario1, "Primer post", [usuario2, usuario3, usuario4])
publicacion4 = (usuario2, "Hola mundo", [usuario1, usuario3, usuario4])
publicacion5 = (usuario1, "Nada", [usuario5])
publicacion6 = (usuario2, "Hola mundo", [usuario1, usuario2])

-- Publicaciones Invalidas

publicacionInvalida = (usuario1,"Hola",(usuarios2))
publicacionInvalida2 = (usuario2, "Chau", [usuarioInvalido1, usuario2])
publicacionInvalida3 = (usuarioInvalido3, "Hola mundo", [usuario1, usuario10, usuarioInvalido1])

publicaciones1 = [(publicacion1), (publicacion2)]
publicaciones2 = [publicacion3, publicacion5]
publicaciones3 = [publicacion1,publicacion2,publicacion3,publicacion4]
publicaciones4 = [publicacion1, publicacion3]
publicaciones5 = [(publicacion2)]

publicacionesInvalidas1 = [(publicacion1), (publicacionInvalida)]
publicacionesInvalidas2 = [(publicacion1), (publicacion1)]
publicacionesInvalidas3 = [(publicacion1), (publicacionInvalida2)]
publicacionesInvalidas4 = [publicacionInvalida3, publicacion3, publicacion4]

-- Redes sociales (si quieren otra red no editen ninguna, hagan otra.)

redSocialA = (usuarios4, relaciones1, [publicacion1])

redSocialB = (usuarios5, relaciones5, [publicacion1])

redSocialC = (usuarios5, relaciones6, [publicacion1])

redSocialD = (usuarios5, relaciones7, publicaciones1)

redSocialE = (usuarios7, relaciones8, publicaciones1)

redSocialF = (usuarios7, relaciones6, [publicacion1])

redSocialG = (usuarios5, relacionesInvalidas3, [publicacion1])

redSocialH = (usuarios2, relaciones1, publicaciones1)

redSocialI = (usuarios1, relacionesInvalidas2, publicaciones1)

redSocialJ = (usuarios1, relaciones1, publicacionesInvalidas2)

redSocialK = (usuarios7, relaciones8, publicaciones4)

redSocialL = (usuarios5, relaciones5, publicaciones2)

redSocialM = (usuarios8, [relacion8], publicaciones1)

redSocialN = (usuarios1, relaciones1, [publicacion5])

redSocialO = (usuarios1, relaciones1, publicaciones1)

redSocialP = (usuarios1, relaciones1, publicaciones3)

redSocialQ = (usuarios1, relaciones1, [publicacion6])

redSocialR = (usuarios9, relaciones1, [publicacion1])
