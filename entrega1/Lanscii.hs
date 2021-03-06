{-|
   /Programa Principal - MiniLogo/


	 Traductores e Interpretadores CI3725 - Grupo 0

	 Versi�n 0.1 2007-05-01


   Grupo 0

	 86-17791 Ernesto Hern�ndez-Novich <mailto:emhn@usb.ve>


	 Programa Principal para el interpretador de MiniLogo.
   Se implantaron algunas funciones auxiliares cuya documentaci�n
	 est� en el texto del programa.

 -}
module Main (
	-- * Funci�n Principal.
  main
) where

import System.IO
import Lexer

{-|
   Funci�n principal.

   El programa puede ser compilado para su ejecuci�n directa,
   o bien cargado en el interpretador GHCi e invocado a trav�s
   de la funci�n @main@.
 -}
main :: IO ()

main =
	do
		fileName <- getFilename
		contents <- readFile fileName
		print $ lexer contents

{-
   getFilename

	 Funci�n auxiliar para obtener el nombre del archivo a procesar.
	 Presenta un prompt en pantalla para que el usuario introduzca
	 el nombre del archivo que desea procesar y espera a que se
	 suministre una l�nea de texto. Esa l�nea de texto es retornada.

	 Note que la documentaci�n de esta funci�n NO aparecer� generada
	 en Haddock pues se trata de una funci�n auxiliar no exportada
	 por el m�dulo.
-}

getFilename =
	do
		hSetBuffering stdout NoBuffering
		putStr "Archivo a Interpretar: "
		fileName <- getLine
		return fileName
