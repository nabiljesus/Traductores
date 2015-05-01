{
module Lexer (lexer) where
}

%wrapper "basic"

$digit = 0-9			-- digits"
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  let					{ \s -> Let }
  in					{ \s -> In }
  $digit+				{ \s -> Int (read s) }
  [\=\+\-\*\/\(\)]			{ \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	Let 		|
	In  		|
	Sym Char	|
	Var String	|
	Int Int
	deriving (Eq,Show)

lexer :: String      -- ^ Cadena de caracteres @S@ a "tokenizar"
         -> [Token]  -- ^ Lista resultante de /tokens/ del tipo @Token@.

lexer s = alexScanTokens s

main = do
  s <- getContents
  print (alexScanTokens s)
}