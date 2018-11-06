module Front.AST where

type Id = String
type VarName = Id
type FuncName = Id
type ArrLen = Int

data Type = IntType
          | ArrType Type (Maybe ArrLen)
          deriving (Eq, Show)

data TypedVar = TypedVar VarName Type
            deriving (Eq, Show)

varName :: TypedVar -> VarName
varName (TypedVar name _) = name

data FuncCall = FuncCall FuncName [IntVal]
              deriving (Eq, Show)

data IntVal = Var VarName
            | Lit Int
            | Result FuncCall
            | ArrAccess VarName IntVal
            | Add IntVal IntVal
            | Sub IntVal IntVal
            | Mult IntVal IntVal
            deriving (Eq, Show)

data BoolVal = TRUE
             | FALSE
             | Eq IntVal IntVal
             | NEq IntVal IntVal
             | Lt IntVal IntVal
             | Gt IntVal IntVal
             | Or BoolVal BoolVal
             | And BoolVal BoolVal
             deriving (Eq, Show)

data Range = IntRange IntVal IntVal
          deriving (Eq, Show)

data DefVal = DefInt IntVal
            | DefArr [IntVal]
            deriving (Eq, Show)

typeOf :: DefVal -> Type
typeOf (DefInt _)  = IntType
typeOf (DefArr xs) = ArrType IntType (Just $ length xs)

data Stm = Return IntVal
         | Def VarName DefVal
         | Assign VarName DefVal
         | AssignArr VarName IntVal IntVal
         | If BoolVal Stm
         | IfElse BoolVal Stm Stm
         | For VarName Range Stm
         | While BoolVal Stm
         | Call FuncCall
         | Comp [Stm]
         | Print IntVal
         | PrintLn
         deriving (Eq, Show)

data FuncDef = FuncDef FuncName [TypedVar] (Maybe Type) Stm
            deriving (Eq, Show)

-- Transforms for loop into while loop.
forAsWhile :: VarName -> Range -> Stm -> Stm
forAsWhile iter (IntRange low high) body =
    let iterVal = Var iter
        cond    = Lt iterVal high -- Iterate while iter < high
        incIter = Assign iter (DefInt (Add (Var iter) (Lit 1))) -- iter++
        body'   = Comp [body, incIter]
        initial = Def iter (DefInt low)
        loop    = While cond body'
    in Comp [initial, loop]

lastStm :: Stm -> Stm
lastStm (Comp ss) = last ss
lastStm stm = stm

isReturn :: Stm -> Bool
isReturn (Return _) = True
isReturn _ = False
