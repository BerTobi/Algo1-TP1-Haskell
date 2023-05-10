import Test.HUnit
import Solucion

main = runTestTT basicosIndependientes >> runTestTT dependientesDeBasicos 

basicosIndependientes = test [
    " siPertenece " ~: (pertenece siPerteneceElem siPerteneceLista) ~?= True,

    " noPertenece " ~: (pertenece noPerteneceElem noPerteneceLista) ~?= False,

    " Pertenece (lista vacia)" ~: (pertenece 1 listaVacia) ~?= False
 ]

dependientesDeBasicos = test [
    " usuariosValidos " ~: (usuariosValidos usuarios1) ~?= False
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Casos

-- Pertenece
siPerteneceElem = 1
siPerteneceLista  = [2, 5, 1, 3, 2]
noPerteneceElem = 'b'
noPerteneceLista = ['z', 'c', 'd', 'a', 'e']
listaVacia = []

-- UsuariosValidos
usuario1 = (1, "Juan")
usuario2 = (0, "Pedro")

usuarios1 = [usuario1, usuario2]