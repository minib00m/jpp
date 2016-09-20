module Parser where
import           Data.Functor.Identity
import           Data.Maybe                             (isJust)
import           Structures
import           Text.Parsec.Prim                       hiding (try)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as T
-----------------LEXER-------------------
lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser emptyDef {
  T.commentStart    = "#-",
  T.commentEnd      = "-#",
  T.identStart      = letter <|> char '_',
  T.identLetter     = alphaNum <|> char '_',
  T.reservedNames   = [
    "true", "false", "not", "and", "or",
    "var", "begin", "end",
    "function", "lambda",
    "if", "else", "then",
    "while", "do", "for",
    "to", "downto",
    "int", "bool",
    "print", "return", "array"
  ],
  T.opStart = oneOf "+-*/:<=>&|![",
  T.reservedOpNames = [
    "+", "-", "*", "**", "/",
    ":=", "&&", "||", "!", "^",
    ">=", "<=", ">", "<", "==", "<>",
    "+=", "-=", "*=", "/=", "**=",
    "++", "--",
    "&=", "|=", "^=",
    "->", "[]"
  ],
  T.caseSensitive   = True
}
variableName :: ParsecT String u Identity String
variableName = T.identifier lexer
keyword      :: String -> ParsecT String u Identity ()
keyword      = T.reserved lexer
operator     :: String -> ParsecT String u Identity ()
operator     = T.reservedOp lexer
symbol       :: String -> ParsecT String u Identity String
symbol       = T.symbol lexer
lexeme       :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme       = T.lexeme lexer
parens       :: ParsecT String u Identity a -> ParsecT String u Identity a
parens       = T.parens lexer
natural      :: ParsecT String u Identity Integer
natural      = T.natural lexer
integer      :: ParsecT String u Identity Integer
integer      = T.integer lexer
semi         :: ParsecT String u Identity String
semi         = T.semi lexer
colon        :: ParsecT String u Identity String
colon        = T.colon lexer
braces       :: ParsecT String u Identity a -> ParsecT String u Identity a
braces       = T.braces lexer
brackets     :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets     = T.brackets lexer
semiSep      :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep      = T.semiSep lexer
semiSep1     :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep1     = T.semiSep1 lexer
commaSep     :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep     = T.commaSep lexer
commaSep1    :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep1    = T.commaSep1 lexer
whiteSpace   :: ParsecT String u Identity ()
whiteSpace   = T.whiteSpace lexer
semiEndBy    :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiEndBy a  = lexeme $ endBy a semi
---------------OPERATOR PARSERS---------------
expression :: Parser Expr
expression = buildExpressionParser exprOpTable exprTerms

exprOpTable :: [[Operator Char st Expr]]
exprOpTable = [
    [Prefix (operator "-" >> return Negate),
     Prefix (operator "!" >> return Not)],
    [Infix  (operator "**" >> return (NumExpr Power          )) AssocRight],
    [Infix  (operator "*"  >> return (NumExpr Multiply       )) AssocLeft,
     Infix  (operator "/"  >> return (NumExpr Divide         )) AssocLeft],
    [Infix  (operator "+"  >> return (NumExpr Plus           )) AssocLeft,
     Infix  (operator "-"  >> return (NumExpr Minus          )) AssocLeft],
    [Infix  (operator "==" >> return (RelExpr Equal          )) AssocNone,
     Infix  (operator "<>" >> return (RelExpr NotEqual       )) AssocNone,
     Infix  (operator ">"  >> return (RelExpr Greater        )) AssocNone,
     Infix  (operator "<"  >> return (RelExpr Less           )) AssocNone,
     Infix  (operator ">=" >> return (RelExpr GreaterEqual   )) AssocNone,
     Infix  (operator "<=" >> return (RelExpr LessEqual      )) AssocNone],
    [Infix  (operator "&&" >> return (BoolExpr And           )) AssocLeft,
     Infix  (operator "||" >> return (BoolExpr Or            )) AssocLeft,
     Infix  (operator  "^" >> return (BoolExpr Xor           )) AssocLeft]
  ]

exprTerms :: Parser Expr
exprTerms =
      parens expression
  <|> try literal
  <|> try array
  <|> try functionCall
  <|> try arrayCall
  <|> try variable
  <|> lambda


boolLiteral :: Parser Expr
boolLiteral =
      (keyword "true" >> return (Lit . BoolLit $ True)  )
  <|> (keyword "false" >> return (Lit . BoolLit $ False))

numLiteral :: Parser Expr
numLiteral = Lit . NumLit <$> integer

literal :: Parser Expr
literal = numLiteral <|> boolLiteral

functionCall :: Parser Expr
functionCall = do
  varName <- variableName
  calls <- many1 (parens $ commaSep expression)
  let packedFunCall = foldl Fun (Single varName) calls
  return $ Var packedFunCall

arrayCall :: Parser Expr
arrayCall = do
  varName <- variableName
  calls <- many1 (brackets expression)
  let packedArrCall = foldl Arr (Single varName) calls
  return $ Var packedArrCall

variable :: Parser Expr
variable = Var . Single <$> variableName

lambda :: Parser Expr
lambda = Lambda <$> functionDef

array :: Parser Expr
array = Array <$> brackets (commaSep expression)

---------------REST OF PARSERS---------------
variableType :: Parser VarType
variableType =
      (TArray <$> (symbol "[" *> variableType <* symbol "|")
              <*> (natural <* symbol "]"))
  <|> TRec <$> parens (commaSep variableType) <*> (operator "->" *> variableType)
  <|> (keyword "int" >> return TInt)
  <|> (keyword "bool" >> return TBool)

variableList :: Parser VarList
variableList = VarList <$> (variableType >>= (commaSep1 .) initVariable)


initVariable :: VarType -> Parser Variable
initVariable varType = do
  varName <- variableName
  maybeAssign <- optionMaybe (operator ":=" *> expression)
  return $ Variable varName maybeAssign varType


statement :: Parser Statement
statement
  =  try ifStatement
 <|> try whileStatement
 <|> try forStatement
 <|> try suffixOperators
 <|> try assignment
 <|> try justExpression
 <|> builtInMethod

justExpression :: Parser Statement
justExpression = JustExpr <$> expression

ifStatement :: Parser Statement
ifStatement = do
  keyword "if"
  ifExpr <- parens expression
  keyword "then"
  seqOfStatements <- body
  elseOption <- optionMaybe $ keyword "else"
  if isJust elseOption then
    If ifExpr seqOfStatements . Just <$> body
  else
    return $ If ifExpr seqOfStatements Nothing


whileStatement :: Parser Statement
whileStatement = While <$> (keyword "while" *> parens expression) <*> (keyword "do" *> body)

forStatement :: Parser Statement
forStatement = For <$> (keyword "for" *> parens forHeader) <*> (keyword "do" *> body)

body :: Parser Statement
body = Seq <$> (keyword "begin" *> semiEndBy statement <* keyword "end")

forHeader :: Parser ForHeader
forHeader = ForHeader <$> (assignment <* semi)
                      <*> (expression <* semi)
                      <*> (try suffixOperators <|> assignment)


assignLHS :: Parser VarCall
assignLHS =
  try (do {(Var ac) <- arrayCall; return ac})
  <|> (Single <$> variableName)

suffixOperators :: Parser Statement
suffixOperators = do
  lhs <- assignLHS
  op <- (operator "++" >> return AAdd) <|> (operator "--" >> return AMinus)
  return $ Assign lhs op (Lit $ NumLit 1)

assignment :: Parser Statement
assignment = Assign <$> assignLHS <*> assignOp <*> expression

assignOp :: Parser AssignOp
assignOp =
      (operator ":=" >> return AMutate)
  <|> (operator "+=" >> return AAdd)
  <|> (operator "-=" >> return AMinus)
  <|> (operator "/=" >> return ADivide)
  <|> (operator "&=" >> return AAnd)
  <|> (operator "|=" >> return AOr)
  <|> (operator "^=" >> return AXor)
  <|> try (operator "*=" >> return AMultiply)
  <|> (operator "**=" >> return APower)


builtInMethod :: Parser Statement
builtInMethod =
      (BuiltInMethod <$> printExpr)
  <|> (Return <$> (keyword "return" *> expression))


printExpr :: Parser BIM
printExpr = Print <$> (keyword "print" *> parens expression)


functionDef :: Parser FunDef
functionDef =
  FunDef <$> functionHeader
         <*> semiEndBy variableList
         <*> semiEndBy functionDef
         <*> body


functionHeader :: Parser FunHeader
functionHeader =
  FunHeader <$> (keyword "function" *> optionMaybe variableName)
            <*> parens (semiSep variableList)
            <*> (operator "->" *> variableType <* semi)

program :: Parser Program
program =
  Program <$> (keyword "program" *> variableName)
          <*> (semi *> semiEndBy variableList)
          <*>  semiEndBy functionDef
          <*>  body
