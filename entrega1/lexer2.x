{
module  Lexer (lexer) where
}

%wrapper "monad"



$spaces = [\ \t]
$alpha = [a-zA-Z]
$digits = [0-9]
$alnum = [$alpha$digits]


@identifier = $alpha $alnum*

@comment = \#.*

@integer = $digits+

@boolean = (true) | (false)

@string = \"[^\"]*\"


:-

@integer    { mkL LInteger }
@boolean    { mkL LBoolean }
@string     { mkL LString }

@identifier  { mkL LIdentifier }

\[@identifier\] { mkL LSection }

=           { mkL LAssign }

\;          { mkL LEndAssign }
@comment    ;
[\ \t \n]+  ;


{

data LexemeClass = LInteger | LBoolean | LString | LIdentifier | LSection | LAssign | LEndAssign | LEOF
    deriving (Eq, Show)


mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = let t = take len str
                           in case c of
                                LInteger -> return (IntegerNum ((read t) :: Integer) p)
                                LBoolean -> return (BooleanVal (if t == "true"
                                                                   then True
                                                                   else False
                                                               ) p)
                                LString -> return (StringTxt (take (length t - 2) (drop 1 t)) p)
                                LIdentifier -> return (Identifier t p)
                                LSection -> return (SectionHeader (take (length t - 2) (drop 1 t)) p)
                                LAssign -> return (Assignment p)
                                LEndAssign -> return (EndAssignment p)


-- No idea why I have to write this myself. Documentation doesn't mention it.
alexEOF :: Alex Token
alexEOF = return Eof



data Token = SectionHeader {identifier :: String, position :: AlexPosn} |
             Identifier {name :: String, position :: AlexPosn}          |
             Assignment {position :: AlexPosn}                          |
             EndAssignment {position :: AlexPosn}                       |
             IntegerNum {value :: Integer, position :: AlexPosn}        |
             BooleanVal {istrue :: Bool, position :: AlexPosn}          |
             StringTxt  {text :: String, position :: AlexPosn}          |
             Eof
    deriving (Eq, Show)


}