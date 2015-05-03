{
{-|
  /Analizador Lexicográfico para MiniLogo/


	Traductores e Interpretadores CI3725

	Versión 0.1 2007-05-17


  Grupo 0

	86-17791 Ernesto Hernández-Novich <mailto:emhn@usb.ve>


	Este módulo, desarrollado en Alex, implanta un Analizador
	Lexicográfico para el lenguaje MiniLogo siguiendo la
	especificación del mismo.

	Se implantaron algunas funciones auxiliares no exportadas,
	cuya documentación se encuentra en el código fuente.
-}
module Lexer (lexer) where
}

%wrapper "posn"

$digito = 0-9           -- UN digito
$letra  = [a-zA-Z]      -- UNA letra
$booleano = [true false]
$canvas = [\<\/\> \<\\\> \<\|\> \<_\> \<\-\>]

tokens :-

	$white+                        ;
	\{                             { \p s -> TkLCurly    s        (lyc p)     }
    --\|                              { \p s -> TkLPipe     s        (lyc p)     }
	\}                             { \p s -> TkRCurly    s        (lyc p)     }
    \%                             { \p s -> TkVarInt    s        (lyc p)     }
    \!                             { \p s -> TkVarBool   s        (lyc p)     }
    @                              { \p s -> TkVarCanvas s        (lyc p)     }
	\[                             { \p s -> TkLB        s        (lyc p)     }
	\]                             { \p s -> TkRB        s        (lyc p)     }
	\;                             { \p s -> TkSeq       s        (lyc p)     }
    \+                             { \p s -> TkSum       s        (lyc p)     }
    \-                             { \p s -> TkMinus     s        (lyc p)     }
    \*                             { \p s -> TkTimes     s        (lyc p)     }
    \/                             { \p s -> TkDiv       s        (lyc p)     }
    \%                             { \p s -> TkMod       s        (lyc p)     }
    \(                             { \p s -> TkLP        s        (lyc p)     }
    \)                             { \p s -> TkRP        s        (lyc p)     }
    --\<                             { \p s -> TkLT        s        (lyc p)     }
    \<\=                           { \p s -> TkLE        s        (lyc p)     }
    --\>                             { \p s -> TkGT        s        (lyc p)     }
    \>\=                           { \p s -> TkGE        s        (lyc p)     }
    \=                             { \p s -> TkEQ        s        (lyc p)     }
    \!\=                           { \p s -> TkNE        s        (lyc p)     }
    $booleano                      { \p s -> TkBool      s        (lyc p)     }
    $canvas                        { \p s -> TkCanvas    s        (lyc p)     }
    $digito+                       { \p s -> TkNum       (read s) (lyc p)     }
    $letra [ $letra $digito _ ]*   { \p s -> TkId        s        (lyc p)     }
    .                              { reportError }
{   

{-|
    El tipo de datos @Token@ representa los diferentes /tokens/
		producidos por el Analizador Lexicográfico. Cada /token/
		va acompañado de una tupla de enteros, cuyos componentes
		denotan la línea y columna, respectivamente, en la cual se
		encontró el /token/ dentro del archivo procesado. Para aquellos
		/tokens/ que lo ameriten, se agrega el lexema convertido al tipo
		deseado, i.e. si es un número se convierte en @Int@ y si es
		una cadenas se convierte en @String@.

		El tipo de datos @Token@ se declara derivando de @Show@ para que
		se pueda probar el Analizador Lexicográfico individualmente, puesto
		que al invocar la función @lexer@ la lista producida será presentada
		directamente en pantalla.

		El tipo de datos @Token@ Se declara derivando de @Eq@ pues es
		requerido para el funcionamiento adecuado del Analizador Sintáctico
		(/parser/).
-}

data Token = TkLCurly   String  (Int,Int) 
        | TkLPipe       String  (Int,Int) 
        | TkRCurly      String  (Int,Int) 
        | TkVarInt      String  (Int,Int) 
        | TkVarBool     String  (Int,Int) 
        | TkVarCanvas   String  (Int,Int) 
        | TkLB          String  (Int,Int) 
        | TkRB          String  (Int,Int) 
        | TkSeq         String  (Int,Int) 
        | TkSum         String  (Int,Int) 
        | TkMinus       String  (Int,Int) 
        | TkTimes       String  (Int,Int) 
        | TkDiv         String  (Int,Int) 
        | TkMod         String  (Int,Int) 
        | TkLP          String  (Int,Int) 
        | TkRP          String  (Int,Int) 
        | TkLT          String  (Int,Int) 
        | TkLE          String  (Int,Int) 
        | TkGT          String  (Int,Int) 
        | TkGE          String  (Int,Int) 
        | TkEQ          String  (Int,Int) 
        | TkNE          String  (Int,Int) 
        | TkBool        String  (Int,Int) 
        | TkCanvas      String  (Int,Int) 
        | TkNum         Int     (Int,Int) 
        | TkId          String  (Int,Int) 
        deriving (Eq, Show)

{-|
    La función @lexer@ encapsula el uso del Analizador Lexicográfico.
		Recibe como único argumento un @String@, presumiblemente leído
		desde un archivo pero también es posible pasarlo explícitamente,
		y produce una lista de /tokens/ a medida que los va procesando.

		En la versión Simple de MiniLogo, todo lo que hace es apoyarse
		en la función @alexScanTokens@ generada por Alex.
-}
lexer :: String      -- ^ Cadena de caracteres @S@ a "tokenizar"
         -> [Token]  -- ^ Lista resultante de /tokens/ del tipo @Token@.

lexer s = alexScanTokens s

{-
    Cada token debe ir acompañado de la línea y columna en la cual
		fue encontrado. El wrapper 'posn' genera para cada token el
		desplazamiento absoluto dentro del archivo, la linea y la columna.
		Como el enunciado del proyecto establece que solamente interesa
		la línea y la columna, la función lyc ("linea y columna") extrae
		solamente la línea y la columna, produciendo una tupla susceptible
		de ser incorporada directamente al token.
 -}
lyc :: AlexPosn -> (Int,Int)
lyc (AlexPn _ l c) = (l,c)

{-
    Cuando ninguna de las expresiones regulares para palabras reservadas,
		símbolos, números o identificadores logra coincidir con la entrada,
		quiere decir que se ha encontrado un caracter inválido para MiniLogo.
		La última expresión regular de la lista captura ese caracter inválido
		y debe emitir un mensaje de error "razonable".

		Cuando se usa el wrapper 'posn' todas las acciones son invocadas
		pasando como parámetros tanto la posición generada por Alex como
		la cadena (en este caso de exactamente un caracter) que coincidió
		con la expresión regular. La función reportError se encarga de emitir
		un mensaje de error apropiado aprovechando esos parámetros.
 -}
reportError :: AlexPosn -> String -> a
reportError p s = error m
	where 
		(l,c) = lyc p
		m     = "\nError (Lexer): Caracter inesperado '" ++ s ++ 
						"' en la linea " ++ (show l) ++ " y columna " ++ (show c) ++ "."
															
}
