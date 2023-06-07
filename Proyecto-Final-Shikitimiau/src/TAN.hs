{-|
Module      : TAN
Description : Modulo que abarca funcioner relacionadas con la Teoría aditiva de los números (TAN)
Copyright   : Diego Angel Rosas Franco, 2023

La Teoría aditiva de los números (TAN), es la representación de los enteros como suma de otros 
enteros.
El presente módulo busca dar un conjunto de funciones útiles en relación a la TAN.
-}
module TAN where

-- Será útil para el uso de listas, en especial para mejorar la complejidad con su uso
import Data.List (nub, permutations, sort, partition)

-- Nos permitirán graficar los datos obtenidos
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-------------------------------------------------------------------------------------------------
-------------------------------------- FUNCIONES AUXILIARES -------------------------------------
-------------------------------------------------------------------------------------------------

-- | tuple3ToList. Convierte una tupla de 3 elementos a una lista de 3 elementos.
tuple3ToList :: (a, a, a) -> [a]
tuple3ToList (a, b, c) = [a, b, c]

-- | tuple3ToList. Convierte una tupla de 4 elementos a una lista de 4 elementos.
tuple4ToList :: (a, a, a, a) -> [a]
tuple4ToList (a, b, c, d) = [a, b, c, d]

-- | esPrimo. Calcula de forma eficiente si un número es primo o no.
esPrimo :: Int -> Bool
esPrimo n
  | n < 2 = False
  | otherwise = Prelude.null [x | x <- [2..intSqrt n], n `mod` x == 0]
  where intSqrt = floor . sqrt . fromIntegral

-------------------------------------------------------------------------------------------------
{-
  El calculo de estas sumas puede llegar a ser ineficiente sin un buen algoritmo que permita 
  calcular las combinaciones, por ello se pensó en una solución como esta:
        cuatroSumas. Función que nos permite obtener una lista con tuplas de 4 elementos
        donde esos 4 elementos sumados dan como resultado al parámetro n
        además, a esos 4 elementos a sumar se les aplica la función func

        cuatroSumas n func = [(a,b,c,d) | a <- [0..intSqrt n], b <- [a..intSqrt n], c <- [b..intSqrt n], d <- [c..intSqrt n],  ((func a) + (func b) + (func c) + (func d)) == n]
        where 
          intSqrt = floor . sqrt . fromIntegral
  El problema es que la última línea (intSqrt) causaría conflicto si es que se quiere usar para numeros Triangulares,  Pentagonales o Poligonales.
  Sin embargo, el trabajar con este tipo de números mas adelante causó problemas igualmente con las funciones gráficas.
  Por ello, decidí dejar la solución de este problema a futuro y concentrarme en la interfaz gráfica para mostrar el uso de
  las funciones que servían y se podían graficar.

-}

-- | cuatroSumas. Función que nos permite obtener una lista con tuplas de 4 elementos
-- donde esos 4 elementos sumados dan como resultado al parámetro n
-- además, a esos 4 elementos a sumar se les aplica la función func
cuatroSumas n func = [(a,b,c,d) | a <- [0..intSqrt n], b <- [a..intSqrt n], c <- [b..intSqrt n], d <- [c..intSqrt n],  ((func a) + (func b) + (func c) + (func d)) == n]
  where 
    intSqrt = floor . sqrt . fromIntegral

-- | tresSumas. Función que nos permite obtener una lista con tuplas de 3 elementos
-- donde esos 3 elementos sumados dan como resultado al parámetro n
-- además, a esos 3 elementos a sumar se les aplica la función func
tresSumas n func = [(a,b,c) | a <- [0..intSqrt n], b <- [a..intSqrt n], c <- [b..intSqrt n],  ((func a) + (func b) + (func c)) == n]
  where 
    intSqrt = floor . sqrt . fromIntegral

-------------------------------------------------------------------------------------------------
------------------------------ FUNCIONES GENERAR NUMEROS POLIGONALES ----------------------------
-------------------------------------------------------------------------------------------------

-- | nT. Función que da el número triangular de orden n.
nT n = (n * (n+1))/2 

-- | nC. Función que da el número cuadrado de orden n.
nC n = n^2

-- | nW. Función que da el número pentagonal de orden n.
nW n = (n * ((3 * n)-1))/2 

-- | nH. Función que da el número hexagonal de orden n.
nH n = n * ((2 * n)-1)

-- | nW. Función que da el k-esimo número poligonal de orden m
-- Comenzamos a partir del orden 3 que corresponde a los triangulares.
poligonal orden k = if orden < 3 
                    then 0
                    else ((m * k * (k - 1))/2) + k
                        where m = orden - 2

-------------------------------------------------------------------------------------------------
------------------------------------------ FUNCIONES TAN ----------------------------------------
-------------------------------------------------------------------------------------------------

{-
  Algunas observaciones y correcciones a futuro.
  Se intentaron definir funciones similares para los otros tipos de números que se definieron.
  Pero se encontraron varios conflictos de tipos que no daba tiempo suficiente para solucionar.
  Por lo que, dejaré esto como una corrección a futuro para enfocarme en poder hacer la conexión
  con la interfaz gráfica que es la otra parte importante.

-}

-------------------------------------------------------------------------------------------------

-- | sumaCuatroCuadrados. Función que nos permite obtener una lista de tuplas de 4 elementos con 
-- los aquellos números que elevados al cuadrado y al sumarse dan como resultado a n.
sumaCuatroCuadrados n = cuatroSumas n nC

-- | cuatroCuadradosPermutationsLength. Permite obtener una lista de tuplas de cuatro elementos
-- que representan las combinaciones sin repeticiones de 4 números que al obtener su cuadrado 
-- y sumarse den n.
sumaCuatroCuadradosPermutations n = nub $ map (sort . tuple4ToList) (sumaCuatroCuadrados n)

-- | cuatroCuadradosPermutationsLength. Permite obtener el número de combinaciones sin repeticiones 
-- de 4 números que al obtener su cuadrado y sumarse den n.
sumaCuatroCuadradosPermutationsLength n = length (sumaCuatroCuadradosPermutations n)

-------------------------------------------------------------------------------------------------

-- | sumaTresCuadrados. Función que nos permite obtener una lista de tuplas de 3 elementos con los 
-- aquellos números que elevados al cuadrado y al sumarse dan como resultado a n.
sumaTresCuadrados n = tresSumas n nC

-- | sumaTresCuadradosPermutations. Permite obtener una lista de tuplas de tres elementos que 
-- representan las combinaciones sin repeticiones de 3 números que al obtener su cuadrado y 
-- sumarse den n.
sumaTresCuadradosPermutations n = nub $ map (sort . tuple3ToList) (sumaTresCuadrados n)

-- | sumaTresCuadradosPermutationsLength. Permite obtener el número de combinaciones sin repeticiones 
-- de 3 números que al obtener su cuadrado y sumarse den n.
sumaTresCuadradosPermutationsLength n = length (sumaTresCuadradosPermutations n)

-------------------------------------------------------------------------------------------------

-- | sumaTresPrimos.  Función que nos permite obtener una lista de tuplas de 3 elementos con los 
-- aquellos números primos que al sumarse dan como resultado a n que además cumple con ser impar.
sumaTresPrimos n
  | odd n = [(a, b, c) | a <- candidatos, b <- candidatos, c <- candidatos, a + b + c == n]
  | otherwise = []
  where
    candidatos = filter esPrimo [0..n]

-- | sumaTresPrimosPermutations.  Función que nos permite obtener las combinaciones sin repeticiones 
-- de tres números primos que al sumarse dan como resultado a n que además cumple con ser impar.
sumaTresPrimosPermutations n = (nub $ map (sort . tuple3ToList) (sumaTresPrimos n))

-- | sumaTresPrimosPermutationsLength. Función que permite obtener el número de combinaciones sin 
-- repeticiones de tres números primos que al sumarse dan como resultado a n que además cumple 
-- con ser impar.
sumaTresPrimosPermutationsLength n = length (sumaTresPrimosPermutations n)


-------------------------------------------------------------------------------------------------
--------------------------------------- FUNCIONES GRÁFICAS --------------------------------------
-------------------------------------------------------------------------------------------------

-- | generarPuntos. Permite generar un conjunto de puntos a graficar a partir de una función y un rango establecido.
-- Parametros:
-- min. Establece el primer valor desde el que se empezará a aplicar la función.
-- max. Establece el valor máximo y el último en el que se aplicará la función.
-- func. Función que será aplicada a cada elemento del rango (min,max)
generarPuntos :: Int -> Int -> (Int -> Int) -> [(Float, Float)]
generarPuntos min max func = [(fromIntegral n, fromIntegral (func n)) | n <- [min..max]]

-- | generarDibujo. Genera un gráfico de puntos a partir de un conjunto de puntos dados.
-- Parametros:
-- puntos. Conjunto de puntos a graficar.
-- ex. Espacio entre los puntos en el eje x.
-- ey. Espacio entre los puntos en el eje y.
-- tc. Tamaño de los circulos.
generarDibujo :: [(Float, Float)] -> Float -> Float -> Float -> Picture
generarDibujo puntos ex ey tc = pictures [translate (x * ex) (y * ey) (circleSolid tc) | (x, y) <- puntos]

-- | generarDibujoColores. Genera un gráfico de puntos a partir de un conjunto de puntos dados.
-- El gráfico además puede iluminar de otro color los puntos que cumplan con una condición dada.
-- Parametros:
-- puntos. Conjunto de puntos a graficar.
-- ex. Espacio entre los puntos en el eje x.
-- ey. Espacio entre los puntos en el eje y.
-- tc. Tamaño de los circulos.
-- func. Función que establece la condición que determinará si el punto se ilumina de otro color.
generarDibujoColores :: [(Float, Float)] -> Float -> Float -> Float -> (Int -> Bool) -> Picture
generarDibujoColores puntos ex ey tc func =
  let (verdes, otros) = partition (\(x, _) -> func (truncate x)) puntos
      dibujoVerdes = pictures [translate (x * ex) (y * ey) (color red (circleSolid tc)) | (x, y) <- verdes]
      dibujoOtros = pictures [translate (x * ex) (y * ey) (color green (circleSolid tc)) | (x, y) <- otros]
  in pictures [dibujoOtros, dibujoVerdes]

-- | imprimeGrafico. Imprime un gráfico con las configuraciones dadas.
-- Las configuraciones se especifican en los parámetros de la función.
-- Parametros:
-- dh. Tamaño horizontal de la ventana a desplegar.
-- dv. Tamaño vertical de la ventana a desplegar.
-- titulo. Titulo de la ventana
-- min. Establece el primer valor desde el que se empezará a aplicar la función a graficar.
-- max. Establece el valor máximo y el último en el que se aplicará la función a graficar.
-- funcGraf. Función a graficar que será aplicada a cada elemento del rango (min,max)
-- ex. Espacio entre los puntos de la gráfica en el eje x.
-- ey. Espacio entre los puntos de la gráfica en el eje y.
-- tc. Tamaño de los circulos de la gráfica.
imprimeGrafico :: Int -> Int -> String -> Int -> Int -> (Int -> Int) -> Float -> Float -> Float -> IO()
imprimeGrafico dh dv titulo min max funcGraf ex ey tc = display (InWindow titulo (dh, dv) (0, 0)) white 
                            (translate (-fromIntegral dh / 2 + 20) (-fromIntegral dv / 2 + 20) 
                            (generarDibujo (generarPuntos min max funcGraf) ex ey tc))


-- | imprimeGraficoCond. Imprime un gráfico con las configuraciones dadas y una condición para
-- establecer de otro color determinados puntos.
-- Las configuraciones se especifican en los parámetros de la función.
-- Parametros:
-- dh. Tamaño horizontal de la ventana a desplegar.
-- dv. Tamaño vertical de la ventana a desplegar.
-- titulo. Titulo de la ventana
-- min. Establece el primer valor desde el que se empezará a aplicar la función a graficar.
-- max. Establece el valor máximo y el último en el que se aplicará la función a graficar.
-- funcGraf. Función a graficar que será aplicada a cada elemento del rango (min,max)
-- ex. Espacio entre los puntos de la gráfica en el eje x.
-- ey. Espacio entre los puntos de la gráfica en el eje y.
-- tc. Tamaño de los circulos de la gráfica.
-- funcCond. Función que establece la condición que determinará si un punto de la gráfica se 
-- ilumina de otro color. 
imprimeGraficoCond :: Int -> Int -> String -> Int -> Int -> (Int -> Int) -> Float -> Float -> Float -> (Int -> Bool) -> IO()
imprimeGraficoCond dh dv titulo min max funcGraf ex ey tc funcCond = display (InWindow titulo (dh, dv) (0, 0)) white 
                            (translate (-fromIntegral dh / 2 + 20) (-fromIntegral dv / 2 + 20) 
                            (generarDibujoColores (generarPuntos min max funcGraf) ex ey tc funcCond))

-------------------------------------------------------------------------------------------------
------------------------------------- FUNCIONES GRÁFICAS TAN ------------------------------------
-------------------------------------------------------------------------------------------------

-- | graficoSumaCuatroCuadrados. Genera un gráfico con la cantidad de representaciones de n como suma de 4 cuadrados en un rango de 0 a 1200. Además ilumina los n primos de otro color.
graficoSumaCuatroCuadrados = imprimeGraficoCond 600 400 "Cantidad de representaciones de n como suma de 4 cuadrados" 0 1200 sumaCuatroCuadradosPermutationsLength 0.3 2 2 esPrimo

-- | graficoSumaTresCuadrados. Genera un gráfico con la cantidad de representaciones de n como suma de 3 cuadrados en un rango de 0 a 1200. Además ilumina los n primos de otro color.
graficoSumaTresCuadrados = imprimeGraficoCond 800 400 "Cantidad de representaciones de n como suma de 3 cuadrados" 0 1200 sumaTresCuadradosPermutationsLength 0.5 6 2 esPrimo

-- | graficoSumaTresPrimos. Genera un gráfico con la cantidad de representaciones de un n impar como suma de 3 primos en un rango de 0 a 1200. Además ilumina los n primos de otro color.
graficoSumaTresPrimos = imprimeGraficoCond 600 400 "Cantidad de representaciones de un n impar como suma de 3 primos" 0 1200 sumaTresPrimosPermutationsLength 0.3 0.2 2 esPrimo

