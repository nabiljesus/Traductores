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
import System.Environment   
import Data.List  
import Text.Printf

{-|
   Funci�n principal.

   El programa puede ser compilado para su ejecuci�n directa,
   o bien cargado en el interpretador GHCi e invocado a trav�s
   de la funci�n @main@.
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
		fileName <- getArgs 
		return $ (fileName !! 0)
