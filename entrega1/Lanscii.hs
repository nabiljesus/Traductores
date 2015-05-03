{-|
   /Programa Principal - MiniLogo/


	 Traductores e Interpretadores CI3725 - Grupo 0

	 Versión 0.1 2007-05-01


   Grupo 0

	 86-17791 Ernesto Hernández-Novich <mailto:emhn@usb.ve>


	 Programa Principal para el interpretador de MiniLogo.
   Se implantaron algunas funciones auxiliares cuya documentación
	 está en el texto del programa.

 -}
module Main (
	-- * Función Principal.
  main
) where

import System.IO
import Lexer
import System.Environment   
import Data.List  
import Text.Printf

{-|
   Función principal.

   El programa puede ser compilado para su ejecución directa,
   o bien cargado en el interpretador GHCi e invocado a través
   de la función @main@.
 -}


check_errors siz tok =
    if tok==[]
        then return ()
    else if (take 1 tok) == []
            then do 
            putStr "vacio wee"
        else check_errors (siz-1) (tail tok)


impresion siz tok =
    if siz==0 then return()
    else if (tok !! 0) /= "Error"
        then
        do
            putStr "token "
            print $ (tok !! 0)
            impresion (siz-1) (tail tok)
        else print "hola"

main :: IO ()

main =
	do
		fileName <- getFilename
		contents <- readFile fileName
		let aux =  lexer contents
		let siz = length(aux)
		tok <- print $ aux
		print $ tok == ""
		

{-
   getFilename

	 Función auxiliar para obtener el nombre del archivo a procesar.
	 Presenta un prompt en pantalla para que el usuario introduzca
	 el nombre del archivo que desea procesar y espera a que se
	 suministre una línea de texto. Esa línea de texto es retornada.

	 Note que la documentación de esta función NO aparecerá generada
	 en Haddock pues se trata de una función auxiliar no exportada
	 por el módulo.
-}

getFilename =
	do
		fileName <- getArgs 
		return $ (fileName !! 0)
