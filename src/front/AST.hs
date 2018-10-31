module Front.AST where

type Id = String
type VarName = Id
type FuncName = Id
type ArrLen = Int

data Type = IntType
          | ArrType Type ArrLen
          deriving (Eq, Show)

data TypedVar = TypedVar VarName Type
            deriving (Eq, Show)

varName :: TypedVar -> VarName
varName (TypedVar name _) = name

data FuncCall = FuncCall FuncName [IntVal]
              deriving (Eq, Show)

data IntTerm = Var VarName
             | Lit Int
             | Result FuncCall
             | ArrAccess VarName IntVal
             | Parens IntVal
             deriving (Eq, Show)

data IntOp = Add  IntTerm
           | Sub  IntTerm
           | Mult IntTerm
           deriving (Eq, Show)

data IntVal = IntVal IntTerm [IntOp]
            deriving (Eq, Show)

data BoolTerm = TRUE
              | FALSE
              | Eq IntVal IntVal
              | NEq IntVal IntVal
              | Lt IntVal IntVal
              | Gt IntVal IntVal
              deriving (Eq, Show)

data BoolOp = Or BoolTerm
            | And BoolTerm
            deriving (Eq, Show)

data BoolVal = BoolVal BoolTerm [BoolOp]
             deriving (Eq, Show)

data Range = IntRange IntVal IntVal
          deriving (Eq, Show)

data DefVal = DefInt IntVal
            | DefArr [IntVal]
            deriving (Eq, Show)

typeOf :: DefVal -> Type
typeOf (DefInt _)  = IntType
typeOf (DefArr xs) = ArrType IntType (length xs)

data Stm = Return IntVal
         | Def VarName DefVal
         | Assign VarName DefVal
         | AssignArr VarName IntVal IntVal
         | If BoolVal Stm
         | IfElse BoolVal Stm Stm
         | For VarName Range Stm
         | While BoolVal Stm
         | Call FuncCall
         | Comp Stm Stm
         | NoOp
         | Print IntVal
         | PrintLn
         deriving (Eq, Show)

data FuncDef = FuncDef FuncName [TypedVar] (Maybe Type) Stm
            deriving (Eq, Show)
