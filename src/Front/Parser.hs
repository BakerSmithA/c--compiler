module Front.Parser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Front.AST

-- Example factorial:
--   def fac(n: Int) -> Int {
--     if n == 0 or n == 1 { return 1 }
--     return fac(n-1) + fac(n-2)
--   }
--
-- Example vector add:
--   def add(xs: [Int], ys: [Int], rs: [Int]) {
--     for let i in 0..10 {
--       rs[i] = xs[i] + ys[i]
--     }
--   }
--
-- Example main:
--   def main() {
--     print(0)
--   }

-- Grammar:
--   IntType          : 'Int'
--   Type             : IntType
--                    | IntType[[IntLiteral]]
--                    | String
--   VarName          : char+
--   IntLiteral       : (0 | .. | 9)+
--
--   IntVal           : VarName
--                    | IntLiteral
--                    | FuncCall
--                    | VarName [ IntVal ]
--                    | IntVal + IntVal
--                    | IntVal - IntVal
--
--   BoolVal          : True
--                    | False
--                    | IntVal == IntVal
--
--   Def              : let VarName = IntVal
--                    | let VarName = [ ArrVals ]
--                    | let VarName = "char*"
--   ArrVals          : ε
--                    | NonEmptyArrVals
--   NonEmptyArrVals  : IntVal ',' NonEmptyArrVals
--                    | IntVal
--
--   If               : if ( BoolVal ) { Stm }
--   IfElse           : if ( BoolVal ) { Stm } else { Stm }
--
--   For              : for IntType VarName in IntLiteral .. IntLiteral { Stm }
--
--   FuncDef          : func FuncName ( FuncArgs ) { Stm } [-> Type]
--   FuncArgs         : ε
--                    | NonEmptyFuncArgs
--   FuncArg          : VarName ':' Type
--   NonEmptyFuncArgs : FuncArg ',' NonEmptyFuncArgs
--                    | FuncArg
--
--   FuncCall         : FuncName ( IntVal* )
--
--   Stm              : return IntVal
--                    | Def
--                    | If
--                    | IfElse
--                    | For
--                    | FuncCall
--                    | Stm \n Stm
--
--   FuncDefs         : FuncDef+

type Parser = Parsec Void String

square :: Parser a -> Parser a
square = between (tok "[") (tok "]")

braces :: Parser a -> Parser a
braces = between (tokNl "{") (tok "}")

parens :: Parser a -> Parser a
parens =  between (tok "(") (tok ")")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (tok ",")

whitespace :: Parser ()
whitespace = many (oneOf "\t ") *> return ()

whitespaceNl :: Parser ()
whitespaceNl = many (oneOf "\t\n ") *> return ()

reservedKeywords :: [String]
reservedKeywords = ["Int", "if", "else", "True", "False", "in", "def", "return", "let", "print", "or", "while"]

-- Checks that the parsed identifier is not a reserved keyword.
reserveCheckedId :: Parser Id -> Parser Id
reserveCheckedId p = (p >>= check) <* whitespace where
    check word = if not (word `elem` reservedKeywords)
                    then return word
                    else fail $ "keyword " ++ show word ++ " cannot be an identifier"

snakeId :: Parser Id
snakeId = reserveCheckedId p where
    p = (:) <$> start <*> many body
    start = lowerChar <|> char '_'
    body = start <|> digitChar

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace or comments.
tok :: String -> Parser String
tok s = chunk s <* whitespace

tokNl :: String -> Parser String
tokNl s = chunk s <* whitespaceNl

num :: Parser Int
num = read <$> some digitChar <* whitespace

singleType :: Parser Type
singleType = IntType <$ tok "Int"

dtype :: Parser Type
dtype = try (ArrType <$> singleType <*> square (optional num))
    <|> singleType

typedVar :: Parser TypedVar
typedVar = TypedVar <$> snakeId <* tok ":" <*> dtype

funcCall :: Parser FuncCall
funcCall = FuncCall <$> snakeId <*> parens (commaSep intVal)

intVal' :: Parser IntVal
intVal' = try (ArrAccess <$> snakeId <*> square intVal)
     <|> try (Result <$> funcCall)
     <|> Var <$> snakeId
     <|> Lit <$> num

intOps :: [[Operator Parser IntVal]]
intOps = [[InfixL (Mult <$ tok "*"), InfixL (Div <$ tok "/")],
          [InfixL (Add <$ tok "+"), InfixL (Sub <$ tok "-")]]

intVal :: Parser IntVal
intVal = makeExprParser intVal' intOps

boolVal' :: Parser BoolVal
boolVal' = TRUE <$ tok "True"
       <|> FALSE <$ tok "False"
       <|> try (Eq <$> intVal <* tok "==" <*> intVal)
       <|> try (NEq <$> intVal <* tok "!=" <*> intVal)
       <|> try (Lt <$> intVal <* tok "<" <*> intVal)
       <|> Gt <$> intVal <* tok ">" <*> intVal

boolOps :: [[Operator Parser BoolVal]]
boolOps = [[InfixL (Or <$ tok "or"), InfixL (And <$ tok "and")]]

boolVal :: Parser BoolVal
boolVal = makeExprParser boolVal' boolOps

range :: Parser Range
range = IntRange <$> intVal <* tok "..<" <*> intVal

defVal :: Parser DefVal
defVal = DefArr <$> square (commaSep intVal)
     <|> DefInt <$> intVal

stm :: Parser Stm
stm = try (Print <$ tok "print" <*> parens intVal)
  <|> try (PrintLn <$ tok "println" <* tok "(" <* tok ")")
  <|> try (Call <$> funcCall)
  <|> Return <$ tok "return" <*> intVal
  <|> Def <$ tok "let" <*> snakeId <* tok "=" <*> defVal
  <|> try (Assign <$> snakeId <* tok "=" <*> defVal)
  <|> try (AssignArr <$> snakeId <*> square intVal <* tok "=" <*> intVal)
  <|> try (IfElse <$ tok "if" <*> boolVal <*> braces stms <* tok "else" <*> braces stms)
  <|> If <$ tok "if" <*> boolVal <*> braces stms
  <|> For <$ tok "for" <* tok "let" <*> snakeId <* tok "in" <*> range <*> braces stms
  <|> While <$ tok "while" <*> boolVal <*> braces stms

stms :: Parser Stm
stms = stm `sepEndBy` (some (tok "\n")) >>= return . comp where
    comp [s] = s
    comp ss = Comp ss

funcDef :: Parser FuncDef
funcDef = FuncDef <$ tok "def" <*> snakeId <*> parens (commaSep typedVar) <*> returnType <*> braces stms where
    returnType = option Nothing (Just <$ tok "->" <*> dtype)

funcDefs :: Parser [FuncDef]
funcDefs = funcDef `sepEndBy` (some (tok "\n"))
