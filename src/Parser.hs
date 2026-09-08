module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = chainl1 intterm addop

addop :: Parser (Exp Int -> Exp Int -> Exp Int)
addop = (reservedOp lis "+" >> return Plus)
  <|> (reservedOp lis "-" >> return Minus)

mulop :: Parser (Exp Int -> Exp Int -> Exp Int)
mulop = (reservedOp lis "*"  >> return Times)
  <|> (reservedOp lis "/" >> return Div)

-- maneja precedencias en mult. y div.
intterm :: Parser (Exp Int)
intterm = chainl1 factor mulop

-- maneja unarios, paréntesis, variables y constantes
factor :: Parser (Exp Int)
factor = try (parens lis intexp)
  <|> (reservedOp lis "-" >> UMinus <$> factor)
  <|> try (
        do
          v <- identifier lis
          reservedOp lis "++"
          return (VarInc v)
      )
  <|> try (
        do
          v <- identifier lis
          reservedOp lis "--"
          return (VarDec v)
      )
  <|> (Var <$> identifier lis)
  <|> (Const . fromInteger <$> natural lis)

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 andterm orop

orop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orop = reservedOp lis "||" >> return Or

andop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andop = reservedOp lis "&&" >> return And

andterm :: Parser (Exp Bool)
andterm = chainl1 boolfactor andop

boolfactor :: Parser (Exp Bool)
boolfactor = try (parens lis boolexp)
  <|> (reservedOp lis "!" >> Not <$> boolfactor)
  <|> (reservedOp lis "true" >> return BTrue)
  <|> (reservedOp lis "false" >> return BFalse)
  <|> relexp

relop :: Parser (Exp Int -> Exp Int -> Exp Bool)
relop = (reservedOp lis "==" >> return Eq)
  <|> (reservedOp lis "!=" >> return NEq)
  <|> (reservedOp lis "<" >> return Lt)
  <|> (reservedOp lis ">" >> return Gt)

-- maneja los operadores que unen dos intexp (2 < 3)
relexp :: Parser (Exp Bool)
relexp = do
  e1 <- intexp
  op <- relop
  e2 <- intexp
  return (op e1 e2)

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 comm1 seqop

seqop :: Parser (Comm -> Comm -> Comm)
seqop = reservedOp lis ";" >> return Seq

comm1 :: Parser Comm
comm1 = (reserved lis "skip" >> return Skip)
  <|> try (
        do -- Usamos try para no consumir la variable si resulta no ser una asignación) 
        v <- identifier lis
        reservedOp lis "="
        e <- intexp
        return (Let v e)
      )
  <|> do 
        reserved lis "if"
        b <- boolexp
        c1 <- braces lis comm -- procesamos llaves con braces { }
        -- Si hay else lo procesa, si no, usa el Skip (o IfThen definido en AST)
        (reserved lis "else" >> braces lis comm >>= \c2 -> return (IfThenElse b c1 c2))
          <|> return (IfThenElse b c1 Skip)
  <|> do
        reserved lis "repeat"
        c <- braces lis comm
        reserved lis "until"
        b <- boolexp
        return (RepeatUntil c b)


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
