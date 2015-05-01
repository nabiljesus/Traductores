{
	module Main (main) where
}

%wrapper "basic"

-- Macros a definir.

$digitos = 0-9							-- Numeros enteros.
$letrasm = [a-z]						-- Letras minusculas.
$letrasM = [A-Z]						-- Letras mayusculas.
$alfabeto = [letrasm letrasM _]			-- Alfabeto completo.

-- Palabra reservada.
--		* "Lanscii"
--		* "read" sirve para obtener datos escritos por el usuario via 
--				 entrada estandar.
--		* "write" sirve para imprimir el lienzo evaluado en una expresion.
@reservado = 
	"Lanscii" | "read" | "write"
		
-- Operadores binarios reservados.
--		* "=" significa asignacion.
--		* ";" significa secuenciacion.
--		* "?" significa sentencia if-then-else.
--		* "|" significa iteracion indeterminada/determinada, 
--			  incorporacion de alcance o concatenacion vertical 
--			  de lienzos.
--		* ".." significa iteracion determinada.
--		* "+" significa suma.
--		* "-" significa resta.
--		* "*" significa producto.
--		* "/" significa division entera.
-- 		* "%" significa modulo.
--		* "\/" significa disjuncion de expresiones booleanas.
--		* "/\" significa conjuncion de expresiones booleanas.
--		* "<" significa "menor que".
--		* "<=" significa "menor o igual que".
--		* ">" significa "mayor que".
--		* ">=" significa "mayor o igual que".
--		* "=" significa igualdad.
--		* "/=" significa diferencia.
--		* ":" significa concatenacion horizontal de lienzos.
-- 		* "$" significa rotacion de lienzos.
--		* "’" significa trasposicion de lienzos.
@opBinReservado =
	"=" | ";" | "?" | "|" | ".." | "+" | "-" | "/" | "*" | "%" | "\/" | "/\"
	| "<" | "<=" | ">" | ">=" | "=" | "/=" | ":" | "$" | "’"

-- Operadores unarios reservados.
--		* "!" significa valor booleano.
--		* "%" significa valor entero.
--		* "@" signigica que es un lienzo para los caracteres ascii:
--			  </> , <\> , <|> , < > , <-> , < > y # (lienzo vacio).
--		* "-" significa negacion de enteros.
--		* "^" significa negacion de expresiones booleanas.
@opUnReservado =
	"!" | "%" | "@" | "-" | "^"
	
	
-- Definiciones del escaner.
tokens :-
	
	-- Definicion de las varaibles.
	$alfabeto [$alfabeto $digitos]*		{ \s -> varT s }
	
	-- Definiciones de los booleanos.
	true								{ boolT }
	false								{ boolT }
	
	-- Definicion de la asignacion.
	
	$white+									;
-- Definiciones del escaner.

{
-- Definiciones de los tipos de datos.
data Token =
	varT				String
	| integerT			Integer
	| boolT				String
	| lienzoT 			String
	| stringT			String
	| opBinReservadoT 	String
	| opUnReservadoT 	String
	deriving (Show)

main = do
  s <- getContents
  print (alexScanTokens s)

}