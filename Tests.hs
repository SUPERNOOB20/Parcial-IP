module Tests where

import Test.HUnit
import Solucion





------- Casos de Test -------
tuki = runTestTT ejercicios
ejercicios = test [ejercicio1, ejercicio2, ejercicio3Testeable, ejercicio4]




ejercicio1 = test [
    votosEnBlanco [("Juan P","Susana G"), ("María M","Pablo M")] [34,56] 100 ~?= 10,
    votosEnBlanco [("Juan P","Susana G"), ("María M","Pablo M")] [50,150] 40000 ~?= 39800
    ]


ejercicio2 = test [
    "Caso con repes y más de una fórmula" ~: formulasValidas [("Doppelganger Martínez", "Neil deGrass Tyson"),("pepito", "Juancho"), ("el peluca", "Doppelganger Martínez")] ~?= False,
    "Caso sin repes y mas de una fórmula" ~: formulasValidas [("Doppelganger Martínez", "Neil deGrass Tyson"),("pepito", "Juancho"), ("el peluca", "Martínez")]              ~?= True,
    "Caso con repes y solo una formula" ~: formulasValidas [("Dictador 1","Dictador 1")] ~?= False,
    "Caso sin repes y solo una fórmula" ~: formulasValidas [("Dictador 1","Dictador 2")] ~?= True
    ]


ejercicio3Testeable = test [
    "% de votos del primer candidato" ~: truncate (porcentajeDeVotos "Juan P" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727]) ~?= 8,
    "% de votos del segundo candidato" ~: truncate (porcentajeDeVotos "María M" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727]) ~?= 1,
    "% de votos del último candidato" ~: truncate (porcentajeDeVotos "Carlos Negri" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727]) ~?= 90,

    "¿suman100%?" ~: porcentajeDeVotos "Juan P" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] + porcentajeDeVotos "María M" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] + porcentajeDeVotos "Carlos Negri" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] ~?= 100.0
    ]


ejercicio4 = test [
    proximoPresidente [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [6,10,1]    ~?= "María M",
    proximoPresidente [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,1]   ~?= "Juan P",
    proximoPresidente [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] ~?= "Carlos Negri",
    proximoPresidente [("Dictador 1","Dictador 2")] [0]                                                          ~?= "Dictador 1"
    ]