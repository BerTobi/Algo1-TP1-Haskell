import Test.HUnit
import Solucion

predicados = runTestTT basicosIndependientes >> runTestTT dependientesDeBasicos 

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

    " UsuariosDeRelacionValidos 2 " ~: (usuariosDeRelacionValidos usuarios4 relaciones2) ~?= False,

    " UsuariosDeRelacionValidos 3 " ~: (usuariosDeRelacionValidos usuarios1 relaciones1) ~?= False,

    " RelacionesAsimetricas 1 " ~: (relacionesAsimetricas relaciones1) ~?= True,

    " RelacionesAsimetricas 2 " ~: (relacionesAsimetricas relaciones3) ~?= False,

    " RelacionesAsimetricas 3 " ~: (relacionesAsimetricas relaciones4) ~?= False,

    " siRelacionadosDirecto 1" ~: (relacionadosDirecto usuario1 usuario2 redSocialA) ~?= True,

    " noRelacionadosDirecto 2" ~: (relacionadosDirecto usuario1 usuario3 redSocialA) ~?= False,

    " noRelacionadosDirecto 3" ~: (relacionadosDirecto usuario5 usuario1 redSocialA) ~?= False
 ]

dependientesDeBasicos = test [
    " usuariosValidos 1" ~: (usuariosValidos usuarios1) ~?= True,

    " usuariosValidos 2" ~: (usuariosValidos usuarios2) ~?= False,

    " usuariosValidos 3" ~: (usuariosValidos usuarios3) ~?= False,

    " cadenaDeAmigos 1" ~: (cadenaDeAmigos usuarios4 redSocialA) ~?= True,

    " cadenaDeAmigos 2" ~: (cadenaDeAmigos usuarios5 redSocialB) ~?= False,

    " cadenaDeAmigos 3" ~: (cadenaDeAmigos listaVacia redSocialA) ~?= False
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

-- Relaciones validas

relacion1 = (usuario1, usuario2)
relacion2 = (usuario4, usuario3)
relacion3 = (usuario2, usuario1)
relacion4 = (usuario3, usuario4)
relacion5 = (usuario1, usuario5)

-- Relaciones invalidas

relacionInvalida1 = (usuario1, usuario1)

relaciones1 = [relacion1, relacion2]
relaciones2 = [relacion1, relacionInvalida1, relacion2]
relaciones3 = [relacion1, relacion2, relacion3, relacion4]
relaciones4 = [relacion1, relacion3, relacion2]

relaciones5 = [relacion5, relacion3]

--Publicaciones
publicacion1 = (usuario1, "Primer post", [usuario2, usuario3, usuario4])
publicacion2 = (usuario2, "Hola mundo", [usuario1, usuario3, usuario4])

-- Redes sociales (si quieren otra red no editen ninguna, hagan otra.)

redSocialA = (usuarios4, relaciones1, [publicacion1])

redSocialB = (usuarios5 ,relaciones5, [publicacion1])