module Solucion where

-- Ejercicio 1


-- Ejemplo: votosEnBlanco [("Juan P","Susana G"), ("María M","Pablo M")] [34, 56] 150  debería devolver 150 - sumaL [34, 56] <----- 60
votosEnBlanco :: [(String, String)] -> [Int] -> Int  -> Int
votosEnBlanco formulas votosValidos votos
    | (sumaL votosValidos) > votos = undefined
    | otherwise = votos - (sumaL votosValidos)

-- Devuelve la suma de todos los números de una lista de enteros.
sumaL :: [Int] -> Int
sumaL []     = 0
sumaL [n]    = n
sumaL (n:ns) = n + sumaL ns

{-
Mis tests para el Ejercicio 1:
votosEnBlanco [("Juan P","Susana G"), ("María M","Pablo M")] [34,56] 100 ---> "10"
votosEnBlanco [("Juan P","Susana G"), ("María M","Pablo M")] [50,150] 40000 ---> "39800"
-}

-- Ejercicio 2
formulasValidas :: [(String, String)] -> Bool
formulasValidas formulas = noHayDuplicados formulas     -- Le hago un "cambio de nombre" a la función, para que sea más declarativo el código

noHayDuplicados :: [(String, String)] -> Bool
noHayDuplicados []        = True
noHayDuplicados [formula] = (fst formula) /= (snd formula)
noHayDuplicados (f:fs)    = ((cantidadDeApariciones (fst f) (f:fs)) <= 1) && ((cantidadDeApariciones (snd f) (f:fs)) <= 1) && (noHayDuplicados fs)
-- f :: (String, String)      //      fs: [(String, String)]

cantidadDeApariciones :: String -> [(String, String)] -> Int
cantidadDeApariciones x [y]
    | x == fst y && x == snd y = 2
    | x == fst y && x /= snd y = 1
    | x /= fst y && x == snd y = 1
    | otherwise                = 0
cantidadDeApariciones x (y:ys)
    | x == fst y && x == snd y = 2 + cantidadDeApariciones x ys
    | x == fst y && x /= snd y = 1 + cantidadDeApariciones x ys
    | x /= fst y && x == snd y = 1 + cantidadDeApariciones x ys
    | otherwise                =     cantidadDeApariciones x ys         -- Caso | x /= fst y && x /= snd y =     cantidadDeApariciones x ys


{-
Mis tests para el Ejercicio 2:
"Caso con repes y más de una fórmula": formulasValidas [("Doppelganger Martínez", "Neil deGrass Tyson"),("pepito", "Juancho"), ("el peluca", "Doppelganger Martínez")] ---> False
"Caso sin repes y mas de una fórmula": formulasValidas [("Doppelganger Martínez", "Neil deGrass Tyson"),("pepito", "Juancho"), ("el peluca", "Martínez")]              ---> True
"Caso con repes y solo una formula": formulasValidas [("Dictador 1","Dictador 1")] ---> False
"Caso sin repes y solo una fórmula": formulasValidas [("Dictador 1","Dictador 2")] ---> True
-}

-- Ejercicio 3

-- ¿Cuenta los votos en blanco? NO, ¡no son datos de entrada siquiera!
-- Dado un candidato a presidente, devuelve el % de votos que obtuvo en ***votos***
-- porcentaje de votos = (cantidad votos presidente * 100) / votos totales
porcentajeDeVotos :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeVotos presidente formulas votos = ((cantidadVotosPresidente) * 100) `division` (sumaL votos)
                                            where cantidadVotosPresidente = devuelvePosicionVotos (posicionPresidente presidente formulas) votos

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b) 

-- Dada una posición, devuelve cuántos votos hay en esa posición.
-- Ejemplos:
-- devuelvePosicionVotos 3 [727,2,7] = 7
-- devuelvePosicionVotos 1 [10,20,727] = 10
devuelvePosicionVotos :: Int -> [Int] -> Int
devuelvePosicionVotos 0 votos  = undefined
devuelvePosicionVotos 1 votos  = head votos
devuelvePosicionVotos i (v:vs) = devuelvePosicionVotos (i - 1) vs

-- Dado un presidente y una lista de fórmulas, devuelve su posición
posicionPresidente :: String -> [(String, String)] -> Int
posicionPresidente presidente [formula] = 1
posicionPresidente presidente (f:fs)
    | presidente == (fst f) = 1
    | otherwise             = 1 + posicionPresidente presidente fs


{-
Mis tests para el Ejercicio 3:
"% de votos del primer candidato": porcentajeDeVotos "Juan P" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] ---> 8.104738...
"% de votos del segundo candidato": porcentajeDeVotos "María M" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] ---> 1.2468828...
"% de votos del último candidato": porcentajeDeVotos "Carlos Negri" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] ---> 90.64838...

"¿suman100%?": porcentajeDeVotos "Juan P" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] + porcentajeDeVotos "María M" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] + porcentajeDeVotos "Carlos Negri" [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] ---> 100.0
-}

-- Ejercicio 4
proximoPresidente :: [(String, String)] -> [Int] -> String      
proximoPresidente formulas votos = presidenteMasVotado formulas votos

presidenteMasVotado :: [(String, String)] -> [Int] -> String
presidenteMasVotado formulas votos = presidenteDePosicion (posicionMaximo votos) formulas 

-- Ejemplos:
-- presidenteDePosicion 2 [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] = María M
-- presidenteDePosicion 1 [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] = Juan P
presidenteDePosicion :: Int -> [(String, String)] -> String
presidenteDePosicion 1 [formula] = fst formula
presidenteDePosicion 1 (f:fs)    = fst f
presidenteDePosicion i (f:fs)    = presidenteDePosicion (i - 1) fs

posicionMaximo :: [Int] -> Int
-- Devuelve la posición donde hay mas votos
-- Ejemplo: fetcheaPosicion [1,50,3] = 2
posicionMaximo votos = matcheaPosicionVotos (maximoL votos (-69)) votos
-- Llamar a maximoL con un contador que valga -69 hará que no interfiera este valor en los cálculos (una cantidad válida de votos debería ser siempre positiva)


-- "maximoL" por "máximo de una lista"
-- Dada una lista, devuelve su valor máximo
-- Ejemplo: [1,2,3,727,1000,3,1,2] = 1000
-- "c" de "contador" (me "guarda" el que por ahora viene siendo el número máximo)
maximoL :: [Int] -> Int -> Int
maximoL []  c = -1      -- No deberíamos llegar a este caso, -1 me indica que hay un error (podría ser también un "undefined").
maximoL [n] c
    | c >= n = c
    | otherwise = n
maximoL (n:ns) c
    | c >= n = maximoL ns c
    | otherwise = maximoL ns n

-- Ejemplo: matcheaPosicionVotos [69,727,11,42] = 2
matcheaPosicionVotos :: Int -> [Int] -> Int
matcheaPosicionVotos n [] = undefined
matcheaPosicionVotos n (v:vs)
    | n == v    = 1
    | otherwise = 1 + matcheaPosicionVotos n vs

{-
Mis casos de test para el ejercicio 4:
proximoPresidente [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [6,10,1]    ---> "María M"
proximoPresidente [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,1]   ---> "Juan P"
proximoPresidente [("Juan P","Susana G"), ("María M","Pablo M"), ("Carlos Negri", "Juan Tetas")] [65,10,727] ---> "Carlos Negri"
proximoPresidente [("Chanta 1","Chanta 2")] [0]                                                              ---> "Chanta 1"
-}