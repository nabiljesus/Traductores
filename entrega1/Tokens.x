-- 
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

--
-- Lexical analyser for Mini Haskell 
--
-- Based on the example Haskell lexer distributed with the alex-2.0
-- distribution, (c) Simon Marlow.
--
-- This file is specifies the token-level aspects of the concrete syntax
-- of Mini Haskell v0.0. The remaining concrete syntax is specified by
-- the Parser.
--
-- Points of some interests:
--      * We don't handle layout
--
-- Design issues:
--      * we explicitly recognise /expression/ syntax, and special
--      chars, so that we can pattern match more easily in the parser.
--      The downside is that you'll have to modify the parser *and* the
--      lexer if you add new syntax.
--

{

{-# OPTIONS -fno-warn-name-shadowing     #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-unused-binds       #-}
{-# OPTIONS -fno-warn-unused-matches     #-}

-- ^ don't want to see all the warns alex templates produce

module Phrac.Lexer ( 
    scan,
    showPos,
    Token(..), Tkn(..)
  ) where

import Phrac.Error ( phasefail_ )

}

%wrapper "posn"

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$digit     = [$ascdigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$symbol    = [$ascsymbol] # [$special \_\:\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff]
$small_    = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small_ $large]

$graphic   = [$small_ $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@varid      = $small $idchar* | $small_ $idchar+
@conid      = $large $idchar*
@varsym     = $symbol $symchar*
@consym     = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

@reservedid = 
    as|case|class|data|default|deriving|do|else|hiding|if|
    import|in|infix|infixl|infixr|instance|let|letrec|module|newtype|
    of|qualified|then|type|where|forall|abstype

@reservedop =
    ".." | ":" | "::" | "=" | ":=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>" | "-" | "@" | "_"

@comment = "--"\-*[^$symbol]?.*
@ws      = $white+ | @comment

phrac :-

<0> @ws                         ;
<0> $special                    { \p s -> T p (SpecialT (head s)) }
<0> @reservedid                 { \p s -> T p (ReservedIdT s) }
<0> @reservedop                 { \p s -> T p (ReservedOpT s) }
<0> @varid                      { \p s -> T p (VarIdT s) }
<0> @conid                      { \p s -> T p (ConIdT s) }
<0> @varsym                     { \p s -> T p (VarSymT s) }
<0> @consym                     { \p s -> T p (ConSymT s) }

<0> @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal          { \p s -> T p (IntegerT (read s)) }

-- <0> @decimal \. @decimal @exponent?
--   | @decimal @exponent          { \p s -> T p (FloatT s) }

<0> \' ($graphic # [\'\\] | " " | @escape) \'   
                                { \p s -> T p (CharT (s!!1)) }

<0> \" @string* \"              { \p s -> T p (StringT s) }

{

data Token = T AlexPosn Tkn
    deriving (Show)

data Tkn
    = VarIdT      String
    | ConIdT      String
    | VarSymT     String
    | ConSymT     String
--  | QVarIdT     String
--  | QConIdT     String
--  | QVarSymT    String
--  | QConSymT    String
    | IntegerT    Integer
--  | FloatT      String
    | CharT       Char
    | StringT     String
    | SpecialT    Char
    | ReservedOpT String
    | ReservedIdT String
    | EOFT
    deriving (Show)

showPos :: AlexPosn -> String
showPos (AlexPn _ l c) = "line " ++ show l ++ ":" ++ show c

scan :: String -> [Token]
scan str = go (alexStartPos,'\n',str)
    where 
      go inp@(pos,_,str) = case alexScan inp 0 of                         
            AlexEOF -> []                            
            AlexError (p,_,s) -> 
                phasefail  "Lexer Error" $ showPos p ++ " `" ++ [head s] ++ "'"
            AlexSkip  inp' len     -> go inp'        
            AlexToken inp' len act -> act pos (take len str) : go inp'

}
