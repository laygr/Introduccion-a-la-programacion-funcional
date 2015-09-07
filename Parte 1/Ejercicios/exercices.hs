module Main where

main =
  do
    solution1
    solution2
    solution3

--------------------------------------------------------------------------
{-
  1.- Escribe una función add que recibe dos números y los suma. En la función solution1, llama a dicha
      función e imprime su resultado.
      Usa la función show para convertir el resultado de add en un String.
      Usa la función putStrLn para imprimir en la consola.
-}
add ... = ...
solution1 = putStrLn (... (add 1 2))
--------------------------------------------------------------------------
{-
  2.- Escribe una funcion solution2 que lee un String x e imprime "Hello, x".
      Usa la función getLine para leer una línea de la consola.
      Usa el operador ++ para concatenar dos strings.
-}
solution2 =
  do
    x <- ...
    putStrLn ("Hello, " ... x)
--------------------------------------------------------------------------
{-
  3.- Escribe una función solution3 que lee dos números x y y e imprime el resultado de su suma.
      Utiliza la función readLn para leer un Int de la consola.
-}
solution3 =
  do
    ... <- ...
    ... <- ...
    putStrLn (show (add x y))
--------------------------------------------------------------------------