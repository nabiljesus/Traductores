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
---$number = [2147483647-2147483647]
$booleano = [true false]
$canvas = [ \/ \\ \| _ \- ]
--$rcommt = [ \-\}]
tokens :-

	$white+                        ;
    \{\-[^\-\}]*\-\}                     ;
    \{                             { \p s -> LCURLY    s        (lyc p)     }
    \|                       { \p s -> PIPE      s        (lyc p)     }
	\}                             { \p s -> RCURLY    s        (lyc p)     }
    \%                             { \p s -> VarInt    s        (lyc p)     }
    \!                             { \p s -> VarBool   s        (lyc p)     }
    @                              { \p s -> AT        s        (lyc p)     }
	\[                             { \p s -> LB        s        (lyc p)     }
	\]                             { \p s -> RB        s        (lyc p)     }
	\;                             { \p s -> SEM_COLON       s        (lyc p)     }
    \:                             { \p s -> COLON       s        (lyc p)     }
    \?                             { \p s -> APOSTROPHE       s        (lyc p)     }
    \'                             { \p s -> QUESTION_MARK       s        (lyc p)     }
    \$                             { \p s -> DOLAR       s        (lyc p)     }
    \+                             { \p s -> SUM       s        (lyc p)     }
    \-                             { \p s -> MINUS     s        (lyc p)     }
    \*                             { \p s -> MULT     s        (lyc p)     }
    \/                             { \p s -> DIV       s        (lyc p)     }
    \%                             { \p s -> PERCENT   s        (lyc p)     }
    \(                             { \p s -> LPARENTHESIS        s        (lyc p)     }
    \)                             { \p s -> RPARENTHESIS        s        (lyc p)     }
    \<                             { \p s -> LTHAN        s        (lyc p)     }
    \<\=                           { \p s -> LEQUAL        s        (lyc p)     }
    \>                             { \p s -> GTHAN        s        (lyc p)     }
    \>\=                           { \p s -> GEQUAL        s        (lyc p)     }
    \=                             { \p s -> EQUALS    s        (lyc p)     }
    \/\=                           { \p s -> NEQUALS        s        (lyc p)     }
    \/\\                           { \p s -> AND        s        (lyc p)     }
    \\\/                           { \p s -> OR        s        (lyc p)     }
    write                          { \p s -> WRITE        s        (lyc p)     }
    read                          { \p s -> READ        s        (lyc p)     }
    $booleano                      { \p s -> BOOLEAN      s        (lyc p)     }
    \<$canvas\>                    { \p s -> CANVAS    s        (lyc p)     }
    \#                             { \p s -> CANVAS    s        (lyc p)     }
    $digito+                       { \p s -> NUMBER       (read s) (lyc p)     }
    $letra [ $letra $digito _ ]*   { \p s -> IDENTIFIER        s        (lyc p)     }
    .                              { \p s -> TkError        s        (lyc p)     }

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

data Token = LCURLY   String  (Int,Int)
        | PIPE        String  (Int,Int)
        | RCURLY      String  (Int,Int)
        | VarInt      String  (Int,Int)
        | VarBool     String  (Int,Int)
        | AT          String  (Int,Int)
        | LB          String  (Int,Int)
        | RB          String  (Int,Int)
        | SEM_COLON         String  (Int,Int)
        | COLON         String  (Int,Int)
        | APOSTROPHE  String (Int,Int)
        | QUESTION_MARK  String (Int,Int)
        | DOLAR  String (Int,Int)
        | SUM         String  (Int,Int)
        | MINUS       String  (Int,Int)
        | MULT       String  (Int,Int)
        | DIV         String  (Int,Int)
        | PERCENT     String  (Int,Int)
        | LPARENTHESIS          String  (Int,Int)
        | RPARENTHESIS          String  (Int,Int)
        | LTHAN          String  (Int,Int)
        | LEQUAL          String  (Int,Int)
        | GTHAN          String  (Int,Int)
        | GEQUAL          String  (Int,Int)
        | EQUALS      String  (Int,Int)
        | AND        String (Int,Int)
        | OR        String (Int,Int)
        | NEQUALS          String  (Int,Int)
        | WRITE       String (Int,Int)
        | READ       String (Int,Int)
        | BOOLEAN        String  (Int,Int)
        | CANVAS      String  (Int,Int)
        | NUMBER         Int     (Int,Int)
        | IDENTIFIER          String  (Int,Int)
        | TkError String (Int,Int)
        deriving (Eq, Show)

{-|
    La función @lexer@ encapsula el uso del Analizador Lexicográfico.
		Recibe como único argumento un @String@, presumiblemente leído
		desde un archivo pero también es posible pasarlo explícitamente,
		y produce una lista de /tokens/ a medida que los va procesando.

		En la versión Simple de MiniLogo, todo lo que hace es apoyarse
		en la función @alexScanTokens@ generada por Alex.
-}
--print :: Token -> Bool
printError (TkError a (b,c)) = 
    do
        print $  "Error (Lexer): Unexpected char '" ++ a ++ 
            "' in line : " ++ (show b) ++ ", column : " ++ (show c) ++ "."

isError :: Token -> Bool
isError (TkError _ _) = True
isError _ = False

check_errors :: [Token]-> Bool
check_errors tok =
    if tok==[] then 
        False
    else 
        if isError (tok !! 0) then True
        else check_errors (tail tok)



impresion tok is =
    if tok==[] then return()
    else do
            if not is then do
                putStr "token "
                print $ (tok !! 0)
                impresion (tail tok) is
            else do
                printError (tok !! 0)
                impresion (tail tok) is
{-
check_errors siz tok =
    if tok==[]
        then do 
        putStr "vacio wee"
    else if (take 1 tok) == []
            then do 
            putStr "vacio wee"
        else return ()
-}

lexer s = do
    let tok = (alexScanTokens s)
    let err = check_errors tok
    --if err then
    if err then impresion (filter (\t -> isError t) tok) True
    else  impresion (filter (\t -> not (isError t)) tok) False
{-
-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act (take len str) : go inp' -}

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



{-reportError :: (Token String AlexPosn) -> String
reportError (a b c) = m
    where 
        (l,c) = lyc c
        m     = "\nError (Lexer): Caracter inesperado '" ++ b ++ 
                        "' en la linea " ++ (show l) ++ " y columna " ++ (show c) ++ "."
-}															
}
